use crate::{
    lexer::Token,
    options::{BeancountOption, BeancountOptionError, ParserOptions},
    types::*,
    ConcreteInput,
};
use chumsky::{input::BorrowInput, label::LabelError, prelude::*};
use either::Either;
use rust_decimal::Decimal;
use std::{
    collections::{hash_map, HashMap, HashSet},
    ops::Deref,
    path::Path,
};
use time::Date;

/// Matches all the includes in the file, ignoring everything else.
pub(crate) fn includes<'src, I>() -> impl Parser<'src, I, Vec<String>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
pub(crate) fn file<'src, I>(
    source_path: &'src Path,
) -> impl Parser<'src, I, Vec<Spanned<Declaration<'src>>>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    declaration(source_path).repeated().collect::<Vec<_>>()
}

/// Matches a [Declaration], and returns with Span.
pub(crate) fn declaration<'src, I>(
    source_path: &'src Path,
) -> impl Parser<'src, I, Spanned<Declaration<'src>>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use Declaration::*;

    choice((
        directive().map(Directive),
        pragma(source_path).map(Pragma),
        ignored_line().map(|_| IgnoredLine),
    ))
    .map_with(spanned_extra)
    .recover_with(skip_then_retry_until(any_ref().ignored(), end()))
}

/// Matches any line starting with any of *:!&#?% which is ignored.
pub(crate) fn ignored_line<'src, I>() -> impl Parser<'src, I, (), Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use Token::*;

    group((
        one_of_ref(&[
            Asterisk,
            Colon,
            DedicatedFlag(Flag::Exclamation),
            DedicatedFlag(Flag::Ampersand),
            Hash,
            DedicatedFlag(Flag::Question),
            DedicatedFlag(Flag::Percent),
        ]),
        (any_ref().and_is(just(Eol).not_ref()))
            .repeated()
            .collect::<Vec<_>>(),
        just(Eol),
    ))
    .ignored()
}

/// Matches a [Directive].
pub(crate) fn directive<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
pub(crate) fn pragma<'src, I>(
    source_path: &'src Path,
) -> impl Parser<'src, I, Pragma<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use Pragma::*;

    choice((
        just(Token::Pushtag)
            .ignore_then(tag())
            .map_with(|tag, e| Pushtag(spanned(tag, e.span()))),
        just(Token::Poptag)
            .ignore_then(tag())
            .map_with(|tag, e| Poptag(spanned(tag, e.span()))),
        just(Token::Pushmeta)
            .ignore_then(meta_key_value())
            .map(Pushmeta),
        just(Token::Popmeta)
            .ignore_then(key())
            .then_ignore(just(Token::Colon))
            .map_with(|key, e| Popmeta(spanned(key, e.span()))),
        just(Token::Include).ignore_then(string()).map(Include),
        option(source_path).map(Pragma::Option),
    ))
    .then_ignore(just(Token::Eol))
    .labelled("directive") // yeah, pragma is not a user-facing concept
    .as_context()
}

/// Matches a [BeancountOption], failing if the option cannot be processed.
pub(crate) fn option<'src, I>(
    source_path: &'src Path,
) -> impl Parser<'src, I, BeancountOption<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    just(Token::Option)
        .ignore_then(string().map_with(|name, e| spanned(name, e.span())))
        .then(string().map_with(|value, e| spanned(value, e.span())))
        .try_map_with(|(name, value), e| {
            use BeancountOptionError::*;

            let opt = BeancountOption::parse(name, value, source_path).map_err(|e| match e {
                UnknownOption => Rich::custom(name.span, e.to_string()),
                BadValue(_) => Rich::custom(value.span, e.to_string()),
            });

            if let Ok(opt) = opt {
                let parser_state: &mut ParserState = e.state();
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
pub(crate) fn transaction<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        transaction_header_line(),
        metadata(),
        posting()
            .map_with(spanned_extra)
            .repeated()
            .collect::<Vec<_>>(),
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

/// Matches the first line of a transaction.
fn transaction_header_line<'src, I>() -> impl Parser<
    'src,
    I,
    (
        Spanned<Date>,
        Spanned<Flag>,
        (Option<Spanned<&'src str>>, Option<Spanned<&'src str>>),
        (
            HashSet<Spanned<&'src Tag<'src>>>,
            HashSet<Spanned<&'src Link<'src>>>,
        ),
    ),
    Extra<'src>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
        }),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
}

/// Matches a price directive, including metadata, over several lines.
pub(crate) fn price<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Price),
        currency().map_with(spanned_extra),
        amount().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn balance<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Balance),
        account().map_with(spanned_extra),
        amount_with_tolerance().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn open<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((open_header_line(), metadata())).validate(
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

/// Matches the first line of a open.
fn open_header_line<'src, I>() -> impl Parser<
    'src,
    I,
    (
        Spanned<Date>,
        Spanned<Account<'src>>,
        HashSet<Spanned<&'src Currency<'src>>>,
        Option<Spanned<Booking>>,
        (
            HashSet<Spanned<&'src Tag<'src>>>,
            HashSet<Spanned<&'src Link<'src>>>,
        ),
    ),
    Extra<'src>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Open),
        account().map_with(spanned_extra),
        currency()
            .map_with(spanned_extra)
            .repeated()
            .collect::<Vec<_>>()
            .validate(|currencies, _span, emitter| {
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
            }),
        booking().map_with(spanned_extra).or_not(),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .map(|(date, _, account, currency, booking, tags_links)| {
        (date, account, currency, booking, tags_links)
    })
}

/// Matches a [Account].
fn account<'src, I>() -> impl Parser<'src, I, Account<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    let account = select_ref!(Token::Account(acc) => acc);

    account.try_map_with(|candidate, e| {
        let span = e.span();
        let parser_state: &ParserState = e.state();
        let account_type_names = &parser_state.options.account_type_names;
        account_type_names
            .get(&candidate.account_type_name)
            .map(|account_type| Account {
                account_type,
                candidate,
            })
            .ok_or(Rich::custom(
                span,
                format!(
                    "unknown account type {}, must be one of {}",
                    &candidate.account_type_name, account_type_names
                ),
            ))
    })
}

/// Matches a [Booking].
fn booking<'src, I>() -> impl Parser<'src, I, Booking, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    string().try_map(|s, span| Booking::try_from(s).map_err(|e| Rich::custom(span, e.to_string())))
}

/// Matches a close, including metadata, over several lines.
pub(crate) fn close<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Close),
        account().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn commodity<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Commodity),
        currency().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn pad<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Pad),
        account().map_with(spanned_extra),
        account().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn document<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Document),
        account().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn note<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Note),
        account().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn event<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Event),
        string().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn query<'src, I>() -> impl Parser<'src, I, Directive<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        date().map_with(spanned_extra),
        just(Token::Query),
        string().map_with(spanned_extra),
        string().map_with(spanned_extra),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .then(metadata())
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
pub(crate) fn txn<'src, I>() -> impl Parser<'src, I, Flag, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    choice((just(Token::Txn).to(Flag::default()), flag()))
}

/// Matches any flag, dedicated or overloaded
pub(crate) fn flag<'src, I>() -> impl Parser<'src, I, Flag, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    let dedicated_flag = select_ref!(Token::DedicatedFlag(flag) => *flag);

    choice((
        dedicated_flag,
        just(Token::Asterisk).to(Flag::Asterisk),
        just(Token::Hash).to(Flag::Hash),
    ))
}

/// Matches a [Posting] complete with [Metadata] over several lines.
fn posting<'src, I>() -> impl Parser<'src, I, Posting<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    just(Token::Indent)
        .ignore_then(
            group((
                flag().map_with(spanned_extra).or_not(),
                account().map_with(spanned_extra),
                expr_value().map_with(spanned_extra).or_not(),
                currency().map_with(spanned_extra).or_not(),
                cost_spec().map_with(spanned_extra).or_not(),
                price_annotation().map_with(spanned_extra).or_not(),
            ))
            .then_ignore(just(Token::Eol))
            .then(metadata())
            .map(
                |((flag, account, amount, currency, cost_spec, price_annotation), metadata)| {
                    Posting {
                        flag,
                        account,
                        amount,
                        currency,
                        cost_spec,
                        price_annotation,
                        metadata,
                    }
                },
            ),
        )
        .labelled("posting")
        .as_context()
}

/// Matches [Metadata], over several lines.
fn metadata<'src, I>() -> impl Parser<'src, I, Metadata<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
    Tag(Spanned<&'a Tag<'a>>),
    Link(Spanned<&'a Link<'a>>),
}

/// Matches a single Metadatum on a single line.
fn meta_key_value<'src, I>() -> impl Parser<'src, I, MetaKeyValue<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    key()
        .map_with(spanned_extra)
        .then(just(Token::Colon).ignore_then(meta_value().map_with(spanned_extra)))
        .map(|(key, value)| MetaKeyValue { key, value })
}

/// Matches a single Metadatum on a single line.
fn metadatum_line<'src, I>() -> impl Parser<'src, I, Metadatum<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
pub(crate) fn meta_value<'src, I>() -> impl Parser<'src, I, MetaValue<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use MetaValue::*;

    choice((simple_value().map(Simple), amount().map(Amount)))
}

/// Matches a [SimpleValue].
/// TODO: the original parser allowed for the SimpleValue to be empty, which we don't support here,
/// unless and until it becomes necessary, because it seems a bit nasty to me. 🤷
pub(crate) fn simple_value<'src, I>() -> impl Parser<'src, I, SimpleValue<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
    ))
}

pub(crate) fn amount<'src, I>() -> impl Parser<'src, I, Amount<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        expr_value().map_with(spanned_extra),
        currency().map_with(spanned_extra),
    ))
    .map(Amount::new)
}

pub(crate) fn amount_with_tolerance<'src, I>(
) -> impl Parser<'src, I, AmountWithTolerance<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        amount().map_with(spanned_extra),
        just(Token::Tilde)
            .ignore_then(decimal().map_with(spanned_extra))
            .or_not(),
    ))
    .map(AmountWithTolerance::new)
}

pub(crate) fn loose_amount<'src, I>() -> impl Parser<'src, I, LooseAmount<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    group((
        expr_value().map_with(spanned_extra).or_not(),
        currency().map_with(spanned_extra).or_not(),
    ))
    .map(LooseAmount::new)
}

pub(crate) fn compound_amount<'src, I>() -> impl Parser<'src, I, ScopedAmount<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use ScopedAmount::*;

    choice((
        (compound_expr().then(currency())).map(|(amount, cur)| CurrencyAmount(amount, cur)),
        compound_expr().map(BareAmount),
        currency().map(BareCurrency),
    ))
}

pub(crate) fn compound_expr<'src, I>() -> impl Parser<'src, I, ScopedExprValue, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use ScopedExprValue::*;

    choice((
        expr_value().then_ignore(just(Token::Hash)).map(PerUnit),
        expr_value().map(PerUnit),
        just(Token::Hash).ignore_then(expr_value()).map(Total),
    ))
}

pub(crate) fn price_annotation<'src, I>() -> impl Parser<'src, I, ScopedAmount<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use ScopedAmount::*;

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
    .try_map(|(is_total, amount, cur), span| match (amount, cur) {
        (Some(amount), Some(cur)) => Ok(CurrencyAmount(scope(amount, is_total), cur)),
        (Some(amount), None) => Ok(BareAmount(scope(amount, is_total))),
        (None, Some(cur)) => Ok(BareCurrency(cur)),
        (None, None) => Err(Rich::custom(span, "empty price annotation")),
    })
}

/// Matches a [CostSpec].
/// For now we only match the new syntax of single braces.
fn cost_spec<'src, I>() -> impl Parser<'src, I, CostSpec<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use self::ScopedAmount::*;
    use CostComp::*;

    just(Token::Lcurl)
        .ignore_then(
            cost_comp()
                .map_with(spanned_extra)
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::Rcurl))
        .try_map(move |cost_comps, span| {
            cost_comps
                .into_iter()
                .fold(
                    // accumulate the `CostComp`s in a `CostSpecBuilder`
                    CostSpecBuilder::default(),
                    |builder, cost_comp| match cost_comp.item {
                        ScopedAmount(compound_amount) => match compound_amount {
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
                .build()
                .map_err(|e| Rich::custom(span, e.to_string()))
        })
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// One component of a cost specification.
/// Setting a field type multiple times is rejected by methods in [CostSpec].
enum CostComp<'a> {
    ScopedAmount(ScopedAmount<'a>),
    Date(Date),
    Label(&'a str),
    Merge,
}

/// Matches one component of a [CostSpec].
fn cost_comp<'src, I>() -> impl Parser<'src, I, CostComp<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    use CostComp::*;

    choice((
        compound_amount().map(ScopedAmount),
        date().map(Date),
        string().map(Label),
        just(Token::Asterisk).to(Merge),
    ))
}

/// Matches zero or more tags or links.
/// Duplicates are errors.
pub(crate) fn tags_links<'src, I>() -> impl Parser<
    'src,
    I,
    (
        HashSet<Spanned<&'src Tag<'src>>>,
        HashSet<Spanned<&'src Link<'src>>>,
    ),
    Extra<'src>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
pub(crate) fn bool<'src, I>() -> impl Parser<'src, I, bool, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    choice((just(Token::True).to(true), just(Token::False).to(false)))
}

/// Match and evaluate an expression
pub(crate) fn expr_value<'src, I>() -> impl Parser<'src, I, ExprValue, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    expr().map(ExprValue::from)
}

/// Match an expression
pub(crate) fn expr<'src, I>() -> impl Parser<'src, I, Expr, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
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
        let factor = just(Minus)
            .or_not()
            .then(number.or(parens.clone()))
            .map(|(negated, x)| {
                if negated.is_some() {
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
fn tag<'src, I>() -> impl Parser<'src, I, &'src Tag<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    select_ref!(Token::Tag(tag) => tag)
}

/// Matches a Link
fn link<'src, I>() -> impl Parser<'src, I, &'src Link<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    select_ref!(Token::Link(link) => link)
}

/// Matches a Key.
/// Note that we may have to hijack another token and use it as a key,
/// since keywords do get used as metadata keys.
fn key<'src, I>() -> impl Parser<'src, I, Key<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    let key = select_ref!(Token::Key(key) => *key);

    choice((
        key,
        keyword()
            .try_map(|s, span| Key::try_from(s).map_err(|e| Rich::custom(span, e.to_string()))),
    ))
}

/// Matches a keyword which has already been parsed as a token.
fn keyword<'src, I>() -> impl Parser<'src, I, &'src str, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    let true_ = select_ref!(Token::True => "TRUE");
    let false_ = select_ref!(Token::False=> "FALSE");
    let null = select_ref!(Token::Null => "NULL");
    let currency = select_ref!(Token::Currency(currency) => currency.as_ref());
    let txn = select_ref!(Token::Txn => "txn");
    let balance = select_ref!(Token::Balance => "balance");
    let open = select_ref!(Token::Open => "open");
    let close = select_ref!(Token::Close => "close");
    let commodity = select_ref!(Token::Commodity => "commodity");
    let pad = select_ref!(Token::Pad => "pad");
    let event = select_ref!(Token::Event => "event");
    let query = select_ref!(Token::Query => "query");
    let custom = select_ref!(Token::Custom => "custom");
    let price = select_ref!(Token::Price => "price");
    let note = select_ref!(Token::Note => "note");
    let document = select_ref!(Token::Document => "document");
    let pushtag = select_ref!(Token::Pushtag => "pushtag");
    let poptag = select_ref!(Token::Poptag => "poptag");
    let pushmeta = select_ref!(Token::Pushmeta => "pushmeta");
    let popmeta = select_ref!(Token::Popmeta => "popmeta");
    let option = select_ref!(Token::Option => "option");
    let options = select_ref!(Token::Options => "options");
    let plugin = select_ref!(Token::Plugin => "plugin");
    let include = select_ref!(Token::Include => "include");

    choice((
        true_, false_, null, currency, txn, balance, open, close, commodity, pad, event, query,
        custom, price, note, document, pushtag, poptag, pushmeta, popmeta, option, options, plugin,
        include,
    ))
}

/// Matches a Currency
fn currency<'src, I>() -> impl Parser<'src, I, &'src Currency<'src>, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    select_ref!(Token::Currency(currency) => currency)
}

/// Matches a Date
fn date<'src, I>() -> impl Parser<'src, I, Date, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    select_ref!(Token::Date(date) => *date)
}

/// Matches a Decimal
fn decimal<'src, I>() -> impl Parser<'src, I, Decimal, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    select_ref!(Token::Number(x) => *x)
}

/// Matches a string
fn string<'src, I>() -> impl Parser<'src, I, &'src str, Extra<'src>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    let string = select_ref!(Token::StringLiteral(s) => s.deref());

    string.map_with(|s, e| {
        let span = e.span();
        let parser_state: &mut ParserState = e.state();
        let ParserState { warnings, options } = parser_state;
        let line_count = s.chars().filter(|c| *c == '\n').count() + 1;
        if line_count > options.long_string_maxlines.item {
            let option_span = options.long_string_maxlines.source.map(|s| s.value);
            let is_default = option_span.is_none();
            let warning = Warning::new(
                "string too long",
                format!(
                    "exceeds long_string_maxlines({}{}) - hint: would require option \"long_string_maxlines\" \"{}\"",
                    if is_default { "default " } else { "" },
                    options.long_string_maxlines.item,
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
    pub(crate) fn merge_tags<E>(&mut self, tags: &HashSet<Spanned<&'a Tag<'a>>>, emitter: &mut E)
    where
        E: Emit<ParserError<'a>>,
    {
        for tag in tags {
            match self.tags.get(tag) {
                None => {
                    self.tags.insert(*tag);
                }
                Some(existing_tag) => {
                    let mut error =
                        Rich::custom(existing_tag.span, format!("duplicate tag {}", tag));
                    LabelError::<ConcreteInput, &str>::in_context(&mut error, "tag", tag.span);
                    emitter.emit(error);
                }
            }
        }
    }

    pub(crate) fn merge_links<E>(&mut self, links: &HashSet<Spanned<&'a Link<'a>>>, emitter: &mut E)
    where
        E: Emit<ParserError<'a>>,
    {
        for link in links {
            match self.links.get(link) {
                None => {
                    self.links.insert(*link);
                }
                Some(existing_link) => {
                    let mut error =
                        Rich::custom(existing_link.span, format!("duplicate link {}", link));
                    LabelError::<ConcreteInput, &str>::in_context(&mut error, "link", link.span);
                    emitter.emit(error);
                }
            }
        }
    }

    pub(crate) fn merge_key_values<E>(
        &mut self,
        key_values: &HashMap<Spanned<Key<'a>>, Spanned<MetaValue<'a>>>,
        emitter: &mut E,
    ) where
        E: Emit<ParserError<'a>>,
    {
        for (key, value) in key_values {
            match self.key_values.get_key_value(key) {
                None => {
                    self.key_values.insert(
                        *key,
                        // Sadly we do have to clone the value here, so we can
                        // merge in metadata key/values from the push/pop stack
                        // without consuming it.
                        value.clone(),
                    );
                }
                Some((existing_key, _existing_value)) => {
                    let mut error =
                        Rich::custom(existing_key.span, format!("duplicate key {}", key));
                    LabelError::<ConcreteInput, &str>::in_context(&mut error, "key", key.span);
                    emitter.emit(error);
                }
            }
        }
    }
}

type ParserError<'a> = Rich<'a, Token<'a>, Span>;

impl<'a> From<ParserError<'a>> for Error {
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
pub(crate) type Extra<'a> = extra::Full<ParserError<'a>, ParserState<'a>, ()>;

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
