use crate::{
    lexer::Token,
    options::{BeancountOption, BeancountOptionError, ParserOptions, DEFAULT_LONG_STRING_MAXLINES},
    types::*,
};
use chumsky::{input::BorrowInput, prelude::*};
use either::Either;
use rust_decimal::Decimal;
use std::{
    collections::{hash_map, HashMap, HashSet},
    iter::once,
    ops::Deref,
    path::Path,
};
use time::Date;

/// Matches all the includes in the file, ignoring everything else.
pub(crate) fn includes<'s, I>() -> impl Parser<'s, I, Vec<String>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    (just(Token::Include).ignore_then(string()).map(Some))
        .or(any_ref().map(|_| None))
        .repeated()
        .collect::<Vec<_>>()
        .map(|includes| {
            includes
                .into_iter()
                .filter_map(|s| s.as_ref().map(|s| s.to_string()))
                .collect::<Vec<_>>()
        })
}

/// Matches the whole file.
pub(crate) fn file<'s, I>(
    source_path: Option<&'s Path>,
) -> impl Parser<'s, I, Vec<Spanned<Declaration<'s>>>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    declaration(source_path).repeated().collect::<Vec<_>>()
}

/// Matches a [Declaration], and returns with Span.
pub(crate) fn declaration<'s, I>(
    source_path: Option<&'s Path>,
) -> impl Parser<'s, I, Spanned<Declaration<'s>>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use Declaration::*;

    choice((directive().map(Directive), pragma(source_path).map(Pragma)))
        .map_with(spanned_extra)
        .recover_with(skip_then_retry_until(any_ref().ignored(), end()))
}

/// Matches a [Directive].
pub(crate) fn directive<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    choice((
        transaction().labelled("transaction").as_context(),
        choice((
            price(),
            balance(),
            open(),
            close(),
            commodity(),
            pad(),
            document(),
            note(),
            event(),
            query(),
            // TODO custom
        ))
        .labelled("directive")
        .as_context(),
    ))
}

/// Matches a [Pragma].
pub(crate) fn pragma<'s, I>(
    source_path: Option<&'s Path>,
) -> impl Parser<'s, I, Pragma<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    choice((
        just(Token::Pushtag)
            .ignore_then(tag())
            .map_with(|tag, e| Pragma::Pushtag(spanned(tag, e.span()))),
        just(Token::Poptag)
            .ignore_then(tag())
            .map_with(|tag, e| Pragma::Poptag(spanned(tag, e.span()))),
        just(Token::Pushmeta)
            .ignore_then(meta_key_value())
            .map(Pragma::Pushmeta),
        just(Token::Popmeta)
            .ignore_then(key())
            .then_ignore(just(Token::Colon))
            .map_with(|key, e| Pragma::Popmeta(spanned(key, e.span()))),
        just(Token::Include)
            .ignore_then(string().map_with(|path, e| Pragma::Include(spanned(path, e.span())))),
        option(source_path).map(Pragma::Option),
        just(Token::Plugin)
            .ignore_then(string().map_with(spanned_extra))
            .then(string().map_with(spanned_extra).or_not())
            .map(|(module_name, config)| {
                Pragma::Plugin(Plugin {
                    module_name,
                    config,
                })
            }),
    ))
    .then_ignore(just(Token::Eol))
    .labelled("directive") // yeah, pragma is not a user-facing concept
    .as_context()
}

/// Matches a [BeancountOption], failing if the option cannot be processed.
pub(crate) fn option<'s, I>(
    source_path: Option<&'s Path>,
) -> impl Parser<'s, I, BeancountOption<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    just(Token::Option)
        .ignore_then(string().map_with(|name, e| spanned(name, e.span())))
        .then(string().map_with(|value, e| spanned(value, e.span())))
        .try_map_with(move |(name, value), e| {
            use BeancountOptionError::*;

            let opt = BeancountOption::parse(name, value, source_path).map_err(|e| match e {
                UnknownOption => Rich::custom(name.span, e.to_string()),
                BadValue(_) => Rich::custom(value.span, e.to_string()),
            });

            if let Ok(opt) = opt {
                let parser_state: &mut extra::SimpleState<ParserState> = e.state();
                parser_state
                    .options
                    .assimilate(opt)
                    .map_err(|e| Rich::custom(value.span, e.to_string()))
            } else {
                opt
            }

            // TODO
            // match parser_options.assimilate(&opt.name, &opt.value) {
            //     Ok(()) => Ok(opt),
            //     // TODO report location of duplicate option
            //     Err(ref e @ DuplicateOption(ref _span)) => {
            //         Err(Rich::custom(name.span, e.to_string()))
            //     }
            //     Err(ref e @ UnknownOption) => Err(Rich::custom(name.span, e.to_string())),
            //     Err(ref e @ BadValue(_)) => Err(Rich::custom(value.span, e.to_string())),
            //     // TODO report location of duplicate value
            //     Err(ref e @ DuplicateValue(ref _span)) => {
            //         Err(Rich::custom(value.span, e.to_string()))
            //     }
            // }
        })
}

/// Matches a transaction, including metadata and postings, over several lines.
pub(crate) fn transaction<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        transaction_header_line(),
        metadata().map_with(spanned_extra),
        posting().repeated().collect::<Vec<_>>(),
    ))
    .validate(
        |((date, flag, (payee, narration), (tags, links)), mut metadata, postings),
         _span,
         emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Transaction(Transaction {
                    flag,
                    payee,
                    narration,
                    postings,
                }),
            }
        },
    )
}

type TransactionHeaderLine<'s> = (
    Spanned<Date>,
    Spanned<Flag>,
    (Option<Spanned<&'s str>>, Option<Spanned<&'s str>>),
    (HashSet<Spanned<Tag<'s>>>, HashSet<Spanned<Link<'s>>>),
);

/// Matches the first line of a transaction.
fn transaction_header_line<'s, I>() -> impl Parser<'s, I, TransactionHeaderLine<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        txn().map_with(spanned_extra),
        // payee and narration get special handling in case one is omitted
        group((
            string().map_with(spanned_extra).or_not(),
            string().map_with(spanned_extra).or_not(),
        ))
        .map(|(s1, s2)| match (s1, s2) {
            // a single string is narration
            (Some(s1), None) => (None, Some(s1)),
            (s1, s2) => (s1, s2),
        })
        .map(|(payee, narration)| {
            (
                replace_some_empty_with_none(payee),
                replace_some_empty_with_none(narration),
            )
        }),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
}

fn replace_some_empty_with_none(s: Option<Spanned<&str>>) -> Option<Spanned<&str>> {
    match s {
        Some(maybe_empty) => {
            if maybe_empty.is_empty() {
                None
            } else {
                s
            }
        }
        None => None,
    }
}

/// Matches a price directive, including metadata, over several lines.
pub(crate) fn price<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Price),
        currency().map_with(spanned_extra),
        amount().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, currency, amount, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);
            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Price(Price { currency, amount }),
            }
        },
    )
    .labelled("price")
    .as_context()
}

/// Matches a balance directive, including metadata, over several lines.
pub(crate) fn balance<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Balance),
        account().map_with(spanned_extra),
        amount_with_tolerance().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, account, atol, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);
            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Balance(Balance { account, atol }),
            }
        },
    )
    .labelled("balance")
    .as_context()
}

/// Matches a open, including metadata, over several lines.
pub(crate) fn open<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((open_header_line(), metadata().map_with(spanned_extra))).validate(
        |((date, account, currencies, booking, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Open(Open {
                    account,
                    currencies,
                    booking,
                }),
            }
        },
    )
}

type OpenHeaderLine<'s> = (
    Spanned<Date>,
    Spanned<Account<'s>>,
    HashSet<Spanned<Currency<'s>>>,
    Option<Spanned<Booking>>,
    (HashSet<Spanned<Tag<'s>>>, HashSet<Spanned<Link<'s>>>),
);

/// Matches the first line of a open.
fn open_header_line<'s, I>() -> impl Parser<'s, I, OpenHeaderLine<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Open),
        account().map_with(spanned_extra),
        currency_list(),
        booking().map_with(spanned_extra).or_not(),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .map(|(date, _, account, currency, booking, tags_links)| {
        (date, account, currency, booking, tags_links)
    })
}

/// Matches zero or more currencies, comma-separated.
fn currency_list<'s, I>() -> impl Parser<'s, I, HashSet<Spanned<Currency<'s>>>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        currency().map_with(spanned_extra),
        (just(Token::Comma).ignore_then(currency().map_with(spanned_extra)))
            .repeated()
            .collect::<Vec<_>>(),
    ))
    .validate(|(first_currency, mut currencies), _span, emitter| {
        currencies.push(first_currency);
        currencies
            .into_iter()
            .fold(HashSet::new(), |mut currencies, currency| {
                if currencies.contains(&currency) {
                    emitter.emit(Rich::custom(
                        currency.span,
                        format!("duplicate currency {}", currency),
                    ))
                } else {
                    currencies.insert(currency);
                }

                currencies
            })
    })
    .or_not()
    .map(|currencies| currencies.unwrap_or_default())
}

/// Matches a [Account].
fn account<'s, I>() -> impl Parser<'s, I, Account<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    let s = select_ref!(Token::Account(s) => *s);

    s.try_map_with(|s, e| {
        let span = e.span();
        //
        // look up the account type name to see which account type it is currently mapped to
        let parser_state: &mut extra::SimpleState<ParserState> = e.state();
        let account_type_names = &parser_state.options.account_type_names;

        Account::new(s, account_type_names).map_err(|e| Rich::custom(span, e.to_string()))
    })
}

/// Matches a [Booking].
fn booking<'s, I>() -> impl Parser<'s, I, Booking, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    string().try_map(|s, span| Booking::try_from(s).map_err(|e| Rich::custom(span, e.to_string())))
}

/// Matches a close, including metadata, over several lines.
pub(crate) fn close<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Close),
        account().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, account, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Close(Close { account }),
            }
        },
    )
}

/// Matches a commodity, including metadata, over several lines.
pub(crate) fn commodity<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Commodity),
        currency().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, currency, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Commodity(Commodity { currency }),
            }
        },
    )
}

/// Matches a pad, including metadata, over several lines.
pub(crate) fn pad<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Pad),
        account().map_with(spanned_extra),
        account().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, account, source, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Pad(Pad { account, source }),
            }
        },
    )
}

/// Matches a document, including metadata, over several lines.
pub(crate) fn document<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Document),
        account().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, account, path, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Document(Document { account, path }),
            }
        },
    )
}

/// Matches a note, including metadata, over several lines.
pub(crate) fn note<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Note),
        account().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, account, comment, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Note(Note { account, comment }),
            }
        },
    )
}

/// Matches an event, including metadata, over several lines.
pub(crate) fn event<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Event),
        string().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, event_type, description, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Event(Event {
                    event_type,
                    description,
                }),
            }
        },
    )
}

/// Matches a query, including metadata, over several lines.
pub(crate) fn query<'s, I>() -> impl Parser<'s, I, Directive<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Query),
        string().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata().map_with(spanned_extra))
    .validate(
        |((date, _, name, content, (tags, links)), mut metadata), _span, emitter| {
            metadata.merge_tags(&tags, emitter);
            metadata.merge_links(&links, emitter);

            Directive {
                date,
                metadata,
                variant: DirectiveVariant::Query(Query { name, content }),
            }
        },
    )
}

/// Matches the `txn` keyword or a flag.
pub(crate) fn txn<'s, I>() -> impl Parser<'s, I, Flag, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    choice((just(Token::Txn).to(Flag::default()), flag()))
}

/// Matches any flag, dedicated or overloaded
pub(crate) fn flag<'s, I>() -> impl Parser<'s, I, Flag, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    let dedicated_flag = select_ref!(Token::DedicatedFlag(flag) => *flag);

    choice((
        dedicated_flag,
        just(Token::Asterisk).to(Flag::Asterisk),
        just(Token::Hash).to(Flag::Hash),
    ))
}

/// Matches a [Posting] complete with [Metadata] over several lines.
fn posting<'s, I>() -> impl Parser<'s, I, Spanned<Posting<'s>>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    just(Token::Indent)
        .ignore_then(
            group((
                flag().map_with(spanned_extra).or_not(),
                account().map_with(spanned_extra),
                expr_value().map_with(spanned_extra).or_not(),
                currency().map_with(spanned_extra).or_not(),
                cost_spec().or_not().map_with(|cost_spec, e| {
                    cost_spec.map(|cost_spec| spanned(cost_spec, e.span()))
                }),
                price_annotation().or_not().map_with(|price_spec, e| {
                    price_spec.map(|price_spec| spanned(price_spec, e.span()))
                }),
            ))
            .map_with(spanned_extra)
            .then_ignore(just(Token::Eol))
            .then(metadata().map_with(spanned_extra))
            .map(
                |(
                    Spanned {
                        item: (flag, account, amount, currency, cost_spec, price_annotation),
                        span: posting_span_without_metadata,
                    },
                    metadata,
                )| {
                    spanned(
                        Posting {
                            flag,
                            account,
                            amount,
                            currency,
                            cost_spec,
                            price_annotation,
                            metadata,
                        },
                        posting_span_without_metadata,
                    )
                },
            ),
        )
        .labelled("posting")
        .as_context()
}

/// Matches [Metadata], over several lines.
fn metadata<'s, I>() -> impl Parser<'s, I, Metadata<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use Metadatum::*;

    metadatum_line()
        .repeated()
        .collect::<Vec<_>>()
        .validate(|metadata, _span, emitter| {
            // collate by type of metadatum
            metadata
                .into_iter()
                .fold(Metadata::default(), |mut m, item| match item {
                    KeyValue(kv) => {
                        use hash_map::Entry::*;

                        let MetaKeyValue { key, value } = kv.item;

                        let key_span = key.span;
                        match m.key_values.entry(key) {
                            Occupied(entry) => emitter.emit(Rich::custom(
                                key_span,
                                format!("duplicate key {}", entry.key()),
                            )),
                            Vacant(entry) => {
                                entry.insert(value);
                            }
                        }

                        m
                    }
                    Tag(tag) => {
                        if m.tags.contains(&tag) {
                            emitter.emit(Rich::custom(tag.span, format!("duplicate tag {}", tag)))
                        } else {
                            m.tags.insert(tag);
                        }

                        m
                    }
                    Link(link) => {
                        if m.links.contains(&link) {
                            emitter
                                .emit(Rich::custom(link.span, format!("duplicate link {}", link)))
                        } else {
                            m.links.insert(link);
                        }

                        m
                    }
                })
        })
}

/// A single instance of [Metadata]
enum Metadatum<'a> {
    KeyValue(Spanned<MetaKeyValue<'a>>),
    Tag(Spanned<Tag<'a>>),
    Link(Spanned<Link<'a>>),
}

/// Matches a single Metadatum on a single line.
fn meta_key_value<'s, I>() -> impl Parser<'s, I, MetaKeyValue<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    key()
        .map_with(spanned_extra)
        .then(just(Token::Colon).ignore_then(meta_value().map_with(spanned_extra)))
        .map(|(key, value)| MetaKeyValue { key, value })
}

/// Matches a single Metadatum on a single line.
fn metadatum_line<'s, I>() -> impl Parser<'s, I, Metadatum<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use Metadatum::*;

    just(Token::Indent)
        .ignore_then(
            choice((
                meta_key_value().map_with(spanned_extra).map(KeyValue),
                tag().map_with(spanned_extra).map(Tag),
                link().map_with(spanned_extra).map(Link),
            ))
            .then_ignore(just(Token::Eol)),
        )
        .labelled("metadata")
        .as_context()
}

/// Matches a [MetaValue].
pub(crate) fn meta_value<'s, I>() -> impl Parser<'s, I, MetaValue<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use MetaValue::*;

    // try for amount first
    choice((amount().map(Amount), simple_value().map(Simple)))
}

/// Matches a [SimpleValue].
pub(crate) fn simple_value<'s, I>() -> impl Parser<'s, I, SimpleValue<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use SimpleValue::*;

    choice((
        string().map(String),
        currency().map(Currency),
        account().map(Account),
        tag().map(Tag),
        link().map(Link),
        date().map(Date),
        bool().map(Bool),
        just(Token::Null).to(None),
        expr_value().map(Expr),
        empty().to(None),
    ))
}

pub(crate) fn amount<'s, I>() -> impl Parser<'s, I, Amount<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        expr_value().map_with(spanned_extra),
        currency().map_with(spanned_extra),
    ))
    .map(Amount::new)
}

pub(crate) fn amount_with_tolerance<'s, I>(
) -> impl Parser<'s, I, AmountWithTolerance<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    choice((
        amount().map_with(|amount, e| AmountWithTolerance::new((spanned_extra(amount, e), None))),
        group((
            expr_value().map_with(spanned_extra),
            just(Token::Tilde),
            decimal().map_with(spanned_extra),
            currency().map_with(spanned_extra),
        ))
        .map_with(|(number, _, tolerance, currency), e| {
            AmountWithTolerance::new((
                spanned_extra(Amount::new((number, currency)), e),
                Some(tolerance),
            ))
        }),
    ))
}

pub(crate) fn loose_amount<'s, I>() -> impl Parser<'s, I, LooseAmount<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    group((
        expr_value().map_with(spanned_extra).or_not(),
        currency().map_with(spanned_extra).or_not(),
    ))
    .map(LooseAmount::new)
}

pub(crate) fn compound_amount<'s, I>() -> impl Parser<'s, I, CompoundAmount<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use CompoundAmount::*;

    choice((
        (compound_expr().then(currency())).map(|(amount, cur)| CurrencyAmount(amount, cur)),
        compound_expr().map(BareAmount),
        just(Token::Hash) // bare currency may or may not be preceeded by hash
            .or_not()
            .ignore_then(currency().map(BareCurrency)),
    ))
}

pub(crate) fn compound_expr<'s, I>() -> impl Parser<'s, I, CompoundExprValue, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use CompoundExprValue::*;

    choice((
        // try for both per-unit and total first
        expr_value()
            .then_ignore(just(Token::Hash))
            .then(expr_value())
            .map(|(per_unit, total)| PerUnitAndTotal(per_unit, total)),
        expr_value().then_ignore(just(Token::Hash)).map(PerUnit),
        expr_value().map(PerUnit),
        just(Token::Hash).ignore_then(expr_value()).map(Total),
    ))
}

pub(crate) fn scoped_expr<'s, I>() -> impl Parser<'s, I, ScopedExprValue, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use ScopedExprValue::*;

    choice((
        expr_value().then_ignore(just(Token::Hash)).map(PerUnit),
        expr_value().map(PerUnit),
        just(Token::Hash).ignore_then(expr_value()).map(Total),
    ))
}

pub(crate) fn price_annotation<'s, I>() -> impl Parser<'s, I, PriceSpec<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use PriceSpec::*;

    fn scope(amount: ExprValue, is_total: bool) -> ScopedExprValue {
        use ScopedExprValue::*;

        if is_total {
            Total(amount)
        } else {
            PerUnit(amount)
        }
    }

    group((
        choice((just(Token::At).to(false), just(Token::AtAt).to(true))),
        expr_value().or_not(),
        currency().or_not(),
    ))
    .try_map(|(is_total, amount, cur), _span| match (amount, cur) {
        (Some(amount), Some(cur)) => Ok(CurrencyAmount(scope(amount, is_total), cur)),
        (Some(amount), None) => Ok(BareAmount(scope(amount, is_total))),
        (None, Some(cur)) => Ok(BareCurrency(cur)),
        (None, None) => Ok(Unspecified),
    })
}

/// Matches a [CostSpec].
/// For now we only match the new syntax of single braces.
fn cost_spec<'s, I>() -> impl Parser<'s, I, CostSpec<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use self::CompoundAmount::*;
    use CostComp::*;

    just(Token::Lcurl)
        .ignore_then(
            group((
                cost_comp().map_with(spanned_extra),
                (just(Token::Comma).ignore_then(cost_comp().map_with(spanned_extra)))
                    .repeated()
                    .collect::<Vec<_>>(),
            ))
            .or_not(), // allow for empty cost spec
        )
        .then_ignore(just(Token::Rcurl))
        .try_map(move |cost_spec, span| {
            let mut builder = match cost_spec {
                Some((head, tail)) => {
                    once(head).chain(tail).fold(
                        // accumulate the `CostComp`s in a `CostSpecBuilder`
                        CostSpecBuilder::default(),
                        |builder, cost_comp| match cost_comp.item {
                            CompoundAmount(compound_amount) => match compound_amount {
                                BareCurrency(cur) => builder.currency(cur, cost_comp.span),
                                BareAmount(amount) => builder.compound_expr(amount, cost_comp.span),
                                CurrencyAmount(amount, cur) => builder
                                    .compound_expr(amount, cost_comp.span)
                                    .currency(cur, cost_comp.span),
                            },
                            Date(date) => builder.date(date, cost_comp.span),
                            Label(s) => builder.label(s, cost_comp.span),
                            Merge => builder.merge(cost_comp.span),
                        },
                    )
                }
                None => CostSpecBuilder::default(),
            };
            builder
                .build()
                .map_err(|e| Rich::custom(span, e.to_string()))
        })
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// One component of a cost specification.
/// Setting a field type multiple times is rejected by methods in [CostSpec].
enum CostComp<'a> {
    CompoundAmount(CompoundAmount<'a>),
    Date(Date),
    Label(&'a str),
    Merge,
}

/// Matches one component of a [CostSpec].
fn cost_comp<'s, I>() -> impl Parser<'s, I, CostComp<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use CostComp::*;

    choice((
        compound_amount().map(CompoundAmount),
        date().map(Date),
        string().map(Label),
        just(Token::Asterisk).to(Merge),
    ))
}

/// Matches zero or more tags or links.
/// Duplicates are errors.
pub(crate) fn tags_links<'s, I>(
) -> impl Parser<'s, I, (HashSet<Spanned<Tag<'s>>>, HashSet<Spanned<Link<'s>>>), Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    choice((
        tag().map_with(spanned_extra).map(Either::Left),
        link().map_with(spanned_extra).map(Either::Right),
    ))
    .repeated()
    .collect::<Vec<_>>()
    .validate(|tags_or_links, _span, emitter| {
        tags_or_links.into_iter().fold(
            (HashSet::new(), HashSet::new()),
            |(mut tags, mut links), item| match item {
                Either::Left(tag) => {
                    if tags.contains(&tag) {
                        emitter.emit(Rich::custom(tag.span, format!("duplicate tag {}", tag)))
                    } else {
                        tags.insert(tag);
                    }

                    (tags, links)
                }
                Either::Right(link) => {
                    if links.contains(&link) {
                        emitter.emit(Rich::custom(link.span, format!("duplicate link {}", link)))
                    } else {
                        links.insert(link);
                    }

                    (tags, links)
                }
            },
        )
    })
}

/// Matches a bool
pub(crate) fn bool<'s, I>() -> impl Parser<'s, I, bool, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    choice((just(Token::True).to(true), just(Token::False).to(false)))
}

/// Match and evaluate an expression
pub(crate) fn expr_value<'s, I>() -> impl Parser<'s, I, ExprValue, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    expr().map(ExprValue::from)
}

/// Match an expression
pub(crate) fn expr<'s, I>() -> impl Parser<'s, I, Expr, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    use Token::*;

    recursive(|expr| {
        // Match a parenthesized expression
        let parens = expr
            .clone()
            .delimited_by(just(Lparen), just(Rparen))
            .map(|x| Expr::Paren(Box::new(x)));

        // Match a bare number
        let number = select_ref! { Number(x) => Expr::Value(*x) };

        // Match a factor of an expression
        let factor = choice((just(Minus), just(Plus)))
            .or_not()
            .then(number.or(parens.clone()))
            .map(|(negated, x)| {
                if negated.is_some_and(|tok| tok == Minus) {
                    Expr::Neg(Box::new(x))
                } else {
                    x
                }
            });

        // Match a product of factors
        let product = factor.clone().foldl(
            choice((
                just(Asterisk).to(Expr::Mul as fn(_, _) -> _),
                just(Slash).to(Expr::Div as fn(_, _) -> _),
            ))
            .then(factor.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        // Match an expression
        product.clone().foldl(
            choice((
                just(Plus).to(Expr::Add as fn(_, _) -> _),
                just(Minus).to(Expr::Sub as fn(_, _) -> _),
            ))
            .then(product.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        )
    })
}

/// Matches a Tag
fn tag<'s, I>() -> impl Parser<'s, I, Tag<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    let tag = select_ref!(Token::Tag(s) => *s);
    tag.try_map(|s, span| {
        TagOrLinkIdentifier::try_from(s)
            .map(Tag)
            .map_err(|e| Rich::custom(span, e.to_string()))
    })
}

/// Matches a Link
fn link<'s, I>() -> impl Parser<'s, I, Link<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    let link = select_ref!(Token::Link(s) => *s);
    link.try_map(|s, span| {
        TagOrLinkIdentifier::try_from(s)
            .map(Link)
            .map_err(|e| Rich::custom(span, e.to_string()))
    })
}

/// Matches a Key.
/// Note that we may have to hijack another token and use it as a key,
/// since keywords do get used as metadata keys.
fn key<'s, I>() -> impl Parser<'s, I, Key<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    let key = select_ref!(Token::Key(s) => *s);

    key.try_map(|s, span| Key::try_from(s).map_err(|e| Rich::custom(span, e.to_string())))
}

/// Matches a Currency
fn currency<'s, I>() -> impl Parser<'s, I, Currency<'s>, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    let currency = select_ref!(Token::Currency(s) => *s);
    currency.try_map(|s, span| Currency::try_from(s).map_err(|e| Rich::custom(span, e.to_string())))
}

/// Matches a Date
fn date<'s, I>() -> impl Parser<'s, I, Date, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    select_ref!(Token::Date(date) => *date)
}

/// Matches a Decimal
fn decimal<'s, I>() -> impl Parser<'s, I, Decimal, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    select_ref!(Token::Number(x) => *x)
}

/// Matches a string
fn string<'s, I>() -> impl Parser<'s, I, &'s str, Extra<'s>>
where
    I: BorrowInput<'s, Token = Token<'s>, Span = Span>,
{
    let string = select_ref!(Token::StringLiteral(s) => s.deref());

    string.map_with(|s, e| {
        let span = e.span();
        let simple_state: &mut extra::SimpleState<ParserState> = e.state();
        let parser_state: &mut ParserState = simple_state;
        let ParserState { warnings, options } = parser_state;
        let line_count = s.chars().filter(|c| *c == '\n').count() + 1;
        let long_string_maxlines = options.long_string_maxlines.as_ref().map(|n| *n.item()).unwrap_or(DEFAULT_LONG_STRING_MAXLINES);
        if line_count > long_string_maxlines {
            let option_span = options.long_string_maxlines.as_ref().map(|s| s.source.value);
            let is_default = option_span.is_none();
            let warning = Warning::new(
                "string too long",
                format!(
                    "exceeds long_string_maxlines({}{}) - hint: would require option \"long_string_maxlines\" \"{}\"",
                    if is_default { "default " } else { "" },
                    long_string_maxlines,
                    line_count
                ),
                span,
            );

            if let Some(option_span) = option_span {
                warnings.push(warning.related_to_named_span("max allowed", option_span));
            } else {
                warnings.push(warning)
            }
        }
        s
    })
}

impl<'a> Metadata<'a> {
    pub(crate) fn merge_tags<E>(&mut self, tags: &HashSet<Spanned<Tag<'a>>>, emitter: &mut E)
    where
        E: Emit<ParserError<'a>>,
    {
        for tag in tags {
            match self.tags.get(tag) {
                None => {
                    self.tags.insert(*tag);
                }
                Some(existing_tag) => {
                    let error = Rich::custom(existing_tag.span, format!("duplicate tag {}", tag));
                    // TODO: label the error in context, type annotations need fixing for chumsky 1.0.0-alpha7 to alpha8 transition
                    // LabelError::<
                    //     chumsky::input::WithContext<
                    //         Span,
                    //         chumsky::input::SpannedInput<Token<'_>, Span, &[(Token<'_>, Span)]>,
                    //     >,
                    //     &str,
                    // >::in_context(&mut error, "tag", tag.span);
                    emitter.emit(error);
                }
            }
        }
    }

    // Augment only for tags which are not already present, others silently ignored.
    // This is so that tags attached to directives take precedence over the push stack.
    pub(crate) fn augment_tags(&mut self, tags: &HashMap<Spanned<Tag<'a>>, Vec<Spanned<Tag<'a>>>>) {
        for (tag, spans) in tags.iter() {
            if !self.tags.contains(tag) {
                let most_recently_pushed_tag = spans.last().unwrap_or(tag);
                self.tags.insert(*most_recently_pushed_tag);
            }
        }
    }

    pub(crate) fn merge_links<E>(&mut self, links: &HashSet<Spanned<Link<'a>>>, emitter: &mut E)
    where
        E: Emit<ParserError<'a>>,
    {
        for link in links {
            match self.links.get(link) {
                None => {
                    self.links.insert(*link);
                }
                Some(existing_link) => {
                    let error =
                        Rich::custom(existing_link.span, format!("duplicate link {}", link));
                    // TODO: label the error in context, type annotations need fixing for chumsky 1.0.0-alpha7 to alpha8 transition
                    // LabelError::<
                    //     chumsky::input::WithContext<
                    //         Span,
                    //         chumsky::input::SpannedInput<Token<'_>, Span, &[(Token<'_>, Span)]>,
                    //     >,
                    //     &str,
                    // >::in_context(&mut error, "link", link.span);
                    emitter.emit(error);
                }
            }
        }
    }

    // Augment only for keys which are not already present, others silently ignored.
    // This is so that key/values attached to directives take precedence over the push stack.
    pub(crate) fn augment_key_values(
        &mut self,
        key_values: &HashMap<Spanned<Key<'a>>, Vec<(Span, Spanned<MetaValue<'a>>)>>,
    ) {
        for (key, values) in key_values {
            if !self.key_values.contains_key(key) {
                let (key_span, value) = values.last().unwrap();
                self.key_values.insert(
                    spanned(*key.item(), *key_span),
                    // Sadly we do have to clone the value here, so we can
                    // merge in metadata key/values from the push/pop stack
                    // without consuming it.
                    value.clone(),
                );
            }
        }
    }
}

type ParserError<'a> = Rich<'a, Token<'a>, Span>;

impl From<ParserError<'_>> for Error {
    fn from(error: ParserError) -> Self {
        let error = error.map_token(|tok| tok.to_string());

        Error::with_contexts(
            error.to_string(),
            error.reason().to_string(),
            *error.span(),
            error
                .contexts()
                .map(|(label, span)| (label.to_string(), *span))
                .collect(),
        )
    }
}

// the state we thread through the parsers
#[derive(Default, Debug)]
pub(crate) struct ParserState<'a> {
    pub(crate) options: ParserOptions<'a>,
    pub(crate) warnings: Vec<Warning>,
}

// our ParserExtra with our error and state types
pub(crate) type Extra<'a> = extra::Full<ParserError<'a>, extra::SimpleState<ParserState<'a>>, ()>;

/// Enable use of own functions which emit errors
pub(crate) trait Emit<E> {
    fn emit(&mut self, err: E);
}

impl<E> Emit<E> for chumsky::input::Emitter<E> {
    fn emit(&mut self, err: E) {
        self.emit(err)
    }
}

// simple collection of errors in a Vec
impl<E> Emit<E> for Vec<Error>
where
    E: Into<Error>,
{
    fn emit(&mut self, err: E) {
        self.push(err.into())
    }
}
// a degenerate error sink
struct NullEmitter;

impl<E> Emit<E> for NullEmitter {
    fn emit(&mut self, _err: E) {}
}

mod tests;
