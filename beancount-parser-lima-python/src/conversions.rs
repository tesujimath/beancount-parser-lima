use std::{cmp::Ordering, collections::HashMap};

use crate::types::*;
use beancount_parser_lima as lima;
use pyo3::{
    prelude::*,
    types::{PyDate, PyDateAccess, PyDict, PyList, PySet, PyString},
};
use string_interner::{symbol::SymbolU32, DefaultStringInterner, StringInterner, Symbol};
use strum::IntoEnumIterator;
use time::Date;

/// Convert from Rust to Python while interning strings, dates, Subaccount lists.
///
/// While PyO3 does seem to support string interning, there's a comment there that
/// every string interning request results in creation of a temporary Python String
/// object, which we choose to avoid.
pub(crate) struct Converter {
    string: StringFactory,
    date: DateFactory,
}

impl Converter {
    pub(crate) fn new() -> Self {
        Converter {
            string: StringFactory::new(),
            date: DateFactory::new(),
        }
    }

    pub(crate) fn directive(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
    ) -> PyResult<Directive> {
        Ok(Directive {
            date: self.date.create_or_reuse(py, date),
            metadata: self.metadata(py, metadata)?,
        })
    }

    pub(crate) fn transaction(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Transaction<'_>,
    ) -> PyResult<Py<PyAny>> {
        let flag = self.flag(py, x.flag().item());
        let payee = x
            .payee()
            .map(|payee| self.string.create_or_reuse(py, payee.item()));
        let narration = x
            .narration()
            .map(|narration| self.string.create_or_reuse(py, narration.item()));
        let postings = x
            .postings()
            .map(|p| self.posting(py, p))
            .collect::<PyResult<Vec<_>>>()?;

        Ok(Py::new(
            py,
            (
                Transaction {
                    flag,
                    payee,
                    narration,
                    postings,
                },
                self.directive(py, date, metadata)?,
            ),
        )?
        .into_any())
    }

    pub(crate) fn posting(&mut self, py: Python<'_>, x: &lima::Posting<'_>) -> PyResult<Posting> {
        let flag = x.flag().map(|flag| self.flag(py, flag.item()));
        let account = self.string.create_or_reuse(py, x.account().item().as_ref());
        let amount = x.amount().map(|amount| amount.value());
        let currency = x
            .currency()
            .map(|currency| self.string.create_or_reuse(py, currency.item().as_ref()));
        let cost_spec = x
            .cost_spec()
            .map(|cost_spec| self.cost_spec(py, cost_spec.item()));
        let price_annotation = x
            .price_annotation()
            .map(|price_annotation| self.price_spec(py, price_annotation.item()));
        let metadata = self.metadata(py, x.metadata())?;

        Ok(Posting {
            flag,
            account,
            amount,
            currency,
            cost_spec,
            price_annotation,
            metadata,
        })
    }

    pub(crate) fn price(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Price<'_>,
    ) -> PyResult<Py<PyAny>> {
        let currency = self
            .string
            .create_or_reuse(py, x.currency().item().as_ref());
        let amount = self.amount(py, x.amount().item());

        Ok(Py::new(
            py,
            (
                Price { currency, amount },
                self.directive(py, date, metadata)?,
            ),
        )?
        .into_any())
    }

    pub(crate) fn balance(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Balance<'_>,
    ) -> PyResult<Py<PyAny>> {
        let account = self.string.create_or_reuse(py, x.account().item().as_ref());
        let atol = self.amount_with_tolerance(py, x.atol());

        Ok(Py::new(
            py,
            (
                Balance { account, atol },
                self.directive(py, date, metadata)?,
            ),
        )?
        .into_any())
    }

    pub(crate) fn open(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Open<'_>,
    ) -> PyResult<Py<PyAny>> {
        let account = self.string.create_or_reuse(py, x.account().item().as_ref());

        let currencies = PyList::new(
            py,
            x.currencies()
                .map(|currency| self.string.create_or_reuse(py, currency.item().as_ref())),
        )?
        .into();

        let booking = x
            .booking()
            .map(|booking| self.string.create_or_reuse(py, booking.item().as_ref()));

        Ok(Py::new(
            py,
            (
                Open {
                    account,
                    currencies,
                    booking,
                },
                self.directive(py, date, metadata)?,
            ),
        )?
        .into_any())
    }

    pub(crate) fn close(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Close<'_>,
    ) -> PyResult<Py<PyAny>> {
        let account = self.string.create_or_reuse(py, x.account().item().as_ref());

        Ok(Py::new(py, (Close { account }, self.directive(py, date, metadata)?))?.into_any())
    }

    pub(crate) fn commodity(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Commodity<'_>,
    ) -> PyResult<Py<PyAny>> {
        let currency = self
            .string
            .create_or_reuse(py, x.currency().item().as_ref());

        Ok(Py::new(
            py,
            (Commodity { currency }, self.directive(py, date, metadata)?),
        )?
        .into_any())
    }

    pub(crate) fn pad(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Pad<'_>,
    ) -> PyResult<Py<PyAny>> {
        let account = self.string.create_or_reuse(py, x.account().item().as_ref());
        let source = self.string.create_or_reuse(py, x.source().item().as_ref());

        Ok(Py::new(
            py,
            (Pad { account, source }, self.directive(py, date, metadata)?),
        )?
        .into_any())
    }

    pub(crate) fn document(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Document<'_>,
    ) -> PyResult<Py<PyAny>> {
        let account = self.string.create_or_reuse(py, x.account().item().as_ref());
        let path = self.string.create_or_reuse(py, x.path().item().as_ref());

        Ok(Py::new(
            py,
            (
                Document { account, path },
                self.directive(py, date, metadata)?,
            ),
        )?
        .into_any())
    }

    pub(crate) fn note(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Note<'_>,
    ) -> PyResult<Py<PyAny>> {
        let account = self.string.create_or_reuse(py, x.account().item().as_ref());
        let comment = self.string.create_or_reuse(py, x.comment().item().as_ref());

        Ok(Py::new(
            py,
            (
                Note { account, comment },
                self.directive(py, date, metadata)?,
            ),
        )?
        .into_any())
    }

    pub(crate) fn event(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Event<'_>,
    ) -> PyResult<Py<PyAny>> {
        let event_type = self
            .string
            .create_or_reuse(py, x.event_type().item().as_ref());
        let description = self
            .string
            .create_or_reuse(py, x.description().item().as_ref());

        Ok(Py::new(
            py,
            (
                Event {
                    event_type,
                    description,
                },
                self.directive(py, date, metadata)?,
            ),
        )?
        .into_any())
    }

    pub(crate) fn query(
        &mut self,
        py: Python<'_>,
        date: &Date,
        metadata: &lima::Metadata,
        x: &lima::Query<'_>,
    ) -> PyResult<Py<PyAny>> {
        let name = self.string.create_or_reuse(py, x.name().item().as_ref());
        let content = self.string.create_or_reuse(py, x.content().item().as_ref());

        Ok(Py::new(
            py,
            (Query { name, content }, self.directive(py, date, metadata)?),
        )?
        .into_any())
    }

    pub(crate) fn metadata(
        &mut self,
        py: Python<'_>,
        x: &lima::Metadata<'_>,
    ) -> Result<Option<Metadata>, PyErr> {
        let key_values = x.key_values();
        let tags = x.tags();
        let links = x.links();

        if key_values.len() == 0 && tags.len() == 0 && links.len() == 0 {
            Ok(None)
        } else {
            let key_values = if key_values.len() == 0 {
                None
            } else {
                let key_value_dict = PyDict::new(py);
                for (k, v) in key_values {
                    key_value_dict.set_item(
                        self.string.create_or_reuse(py, k.item().as_ref()),
                        self.meta_value(py, v.item())?,
                    )?
                }
                Some(key_value_dict.into())
            };

            let tags = if tags.len() == 0 {
                None
            } else {
                Some(
                    PyList::new(
                        py,
                        tags.map(|tag| self.string.create_or_reuse(py, tag.item().as_ref())),
                    )?
                    .into(),
                )
            };

            let links = if links.len() == 0 {
                None
            } else {
                Some(
                    PyList::new(
                        py,
                        links.map(|link| self.string.create_or_reuse(py, link.item().as_ref())),
                    )?
                    .into(),
                )
            };

            Ok(Some(Metadata {
                key_values,
                tags,
                links,
            }))
        }
    }

    pub(crate) fn meta_value(
        &mut self,
        py: Python<'_>,
        x: &lima::MetaValue<'_>,
    ) -> PyResult<Py<PyAny>> {
        use lima::MetaValue::*;
        use lima::SimpleValue::*;

        let meta_value = MetaValue {};

        match x {
            Simple(String(x)) => {
                let value = self.string.create_or_reuse(py, x);
                Ok(Py::new(py, (MetaValueString { value }, meta_value))?.into_any())
            }

            Simple(Currency(x)) => {
                let value = self.string.create_or_reuse(py, x.as_ref());
                Ok(Py::new(py, (MetaValueCurrency { value }, meta_value))?.into_any())
            }

            Simple(Account(x)) => {
                let value = self.string.create_or_reuse(py, x.as_ref());
                Ok(Py::new(py, (MetaValueAccount { value }, meta_value))?.into_any())
            }

            Simple(Tag(x)) => {
                let value = self.string.create_or_reuse(py, x.as_ref());
                Ok(Py::new(py, (MetaValueTag { value }, meta_value))?.into_any())
            }

            Simple(Link(x)) => {
                let value = self.string.create_or_reuse(py, x.as_ref());
                Ok(Py::new(py, (MetaValueLink { value }, meta_value))?.into_any())
            }

            Simple(Date(x)) => {
                let value = self.date.create_or_reuse(py, x);
                Ok(Py::new(py, (MetaValueDate { value }, meta_value))?.into_any())
            }

            Simple(Bool(x)) => {
                let value = *x;
                Ok(Py::new(py, (MetaValueBool { value }, meta_value))?.into_any())
            }

            Simple(None) => Ok(Py::new(py, (MetaValueNone, meta_value))?.into_any()),

            Simple(Expr(x)) => {
                let value = x.value();
                Ok(Py::new(py, (MetaValueExpr { value }, meta_value))?.into_any())
            }

            Amount(x) => {
                let value = self.amount(py, x);
                Ok(Py::new(py, (MetaValueAmount { value }, meta_value))?.into_any())
            }
        }
    }

    pub(crate) fn flag(&mut self, py: Python<'_>, x: &lima::Flag) -> Py<PyString> {
        use lima::Flag::*;

        let mut buf = [0; 8]; // two unicode characters, more than we need for an ASCII flag mpoe.g. 'A
        let s = match x {
            Asterisk => '*'.encode_utf8(&mut buf),
            Exclamation => '*'.encode_utf8(&mut buf),
            Ampersand => '*'.encode_utf8(&mut buf),
            Hash => '*'.encode_utf8(&mut buf),
            Question => '*'.encode_utf8(&mut buf),
            Percent => '*'.encode_utf8(&mut buf),
            Letter(x) => {
                let quote_len = '\''.encode_utf8(&mut buf).len();
                let char_len = x.char().encode_utf8(&mut buf[quote_len..]).len();
                std::str::from_utf8(&buf[..quote_len + char_len]).unwrap()
            }
        };
        self.string.create_or_reuse(py, s)
    }

    pub(crate) fn amount(&mut self, py: Python<'_>, x: &lima::Amount) -> Amount {
        let number = x.number().item().value();
        let currency = self
            .string
            .create_or_reuse(py, x.currency().item().as_ref());
        Amount { number, currency }
    }

    pub(crate) fn amount_with_tolerance(
        &mut self,
        py: Python<'_>,
        x: &lima::AmountWithTolerance,
    ) -> AmountWithTolerance {
        let amount = self.amount(py, x.amount());
        let tolerance = x.tolerance().map(|tolerance| *tolerance.item());
        AmountWithTolerance { amount, tolerance }
    }

    pub(crate) fn cost_spec(&mut self, py: Python<'_>, x: &lima::CostSpec<'_>) -> CostSpec {
        let per_unit = x.per_unit().map(|per_unit| per_unit.item().value());
        let total = x.total().map(|total| total.item().value());
        let currency = x
            .currency()
            .map(|currency| self.string.create_or_reuse(py, currency.item().as_ref()));
        let date = x.date().map(|date| self.date.create_or_reuse(py, date));

        let label = x
            .label()
            .map(|label| self.string.create_or_reuse(py, label.item().as_ref()));
        let merge = x.merge();

        CostSpec {
            per_unit,
            total,
            currency,
            date,
            label,
            merge,
        }
    }

    pub(crate) fn price_spec(&mut self, py: Python<'_>, x: &lima::PriceSpec<'_>) -> PriceSpec {
        use lima::PriceSpec::*;
        use lima::ScopedExprValue::*;

        let per_unit = match x {
            BareAmount(PerUnit(expr)) => Some(expr.value()),
            CurrencyAmount(PerUnit(expr), _) => Some(expr.value()),
            _ => None,
        };

        let total = match x {
            BareAmount(Total(expr)) => Some(expr.value()),
            CurrencyAmount(Total(expr), _) => Some(expr.value()),
            _ => None,
        };

        let currency = match x {
            BareCurrency(currency) => Some(*currency),
            CurrencyAmount(_, currency) => Some(*currency),
            _ => None,
        }
        .map(|currency| self.string.create_or_reuse(py, currency.as_ref()));

        PriceSpec {
            per_unit,
            total,
            currency,
        }
    }

    pub(crate) fn options(&mut self, py: Python<'_>, x: &lima::Options<'_>) -> PyResult<Options> {
        let title = self.string.create_or_reuse(py, x.title());
        let account_previous_balances = self
            .string
            .create_or_reuse(py, x.account_previous_balances().as_ref());
        let account_previous_earnings = self
            .string
            .create_or_reuse(py, x.account_previous_earnings().as_ref());
        let account_previous_conversions = self
            .string
            .create_or_reuse(py, x.account_previous_conversions().as_ref());
        let account_current_earnings = self
            .string
            .create_or_reuse(py, x.account_current_earnings().as_ref());
        let account_current_conversions = self
            .string
            .create_or_reuse(py, x.account_current_conversions().as_ref());
        let account_unrealized_gains = self
            .string
            .create_or_reuse(py, x.account_unrealized_gains().as_ref());
        let account_rounding = x
            .account_rounding()
            .map(|account_rounding| self.string.create_or_reuse(py, account_rounding.as_ref()));
        let conversion_currency = self
            .string
            .create_or_reuse(py, x.conversion_currency().as_ref());

        let inferred_tolerance_default = PyDict::new(py);
        for (c, d) in x.inferred_tolerance_defaults() {
            // let c = match c {
            //     Some(c) => (Some(self.string.create_or_reuse(py, c.as_ref())), d),
            //     None => (None, d),
            // };
            // inferred_tolerance_default.set_item(c, d)?;
            inferred_tolerance_default
                .set_item(c.map(|c| self.string.create_or_reuse(py, c.as_ref())), d)?
        }
        let inferred_tolerance_default = inferred_tolerance_default.into();

        let inferred_tolerance_multiplier = x.inferred_tolerance_multiplier();
        let infer_tolerance_from_cost = x.infer_tolerance_from_cost();

        let documents = PySet::new(
            py,
            x.documents()
                .map(|p| {
                    self.string
                        .create_or_reuse(py, p.to_string_lossy().as_ref())
                })
                .collect::<Vec<Py<PyString>>>()
                .iter(),
        )
        .unwrap()
        .into();

        let operating_currency = PySet::new(
            py,
            x.operating_currency()
                .map(|c| self.string.create_or_reuse(py, c.as_ref()))
                .collect::<Vec<_>>()
                .iter(),
        )
        .unwrap()
        .into();
        let render_commas = x.render_commas();
        let booking_method = self.string.create_or_reuse(py, x.booking_method().as_ref());
        let plugin_processing_mode = self
            .string
            .create_or_reuse(py, x.plugin_processing_mode().into());

        let account_type_name = lima::AccountType::iter()
            .map(|t| (t, x.account_type_name(t)))
            .collect::<HashMap<lima::AccountType, &lima::AccountTypeName>>();

        let account_name_by_type = PyDict::new(py);
        let account_type_by_name = PyDict::new(py);
        for (t, n) in account_type_name {
            account_name_by_type.set_item(
                self.string.create_or_reuse(py, t.as_ref()),
                self.string.create_or_reuse(py, n.as_ref()),
            )?;
            account_type_by_name.set_item(
                self.string.create_or_reuse(py, n.as_ref()),
                self.string.create_or_reuse(py, t.as_ref()),
            )?;
        }
        let account_name_by_type = account_name_by_type.into();
        let account_type_by_name = account_type_by_name.into();

        let long_string_maxlines = x.long_string_maxlines();

        Ok(Options {
            title,
            account_previous_balances,
            account_previous_earnings,
            account_previous_conversions,
            account_current_earnings,
            account_current_conversions,
            account_unrealized_gains,
            account_rounding,
            conversion_currency,
            inferred_tolerance_default,
            inferred_tolerance_multiplier,
            infer_tolerance_from_cost,
            documents,
            operating_currency,
            render_commas,
            booking_method,
            plugin_processing_mode,
            account_name_by_type,
            account_type_by_name,
            long_string_maxlines,
        })
    }

    pub(crate) fn plugin(&mut self, py: Python<'_>, x: &lima::Plugin<'_>) -> PyResult<Plugin> {
        let module_name = self.string.create_or_reuse(py, x.module_name());
        let config = x
            .config()
            .as_ref()
            .map(|config| self.string.create_or_reuse(py, config.item()));

        Ok(Plugin {
            module_name,
            config,
        })
    }
}

struct StringFactory {
    string_interner: DefaultStringInterner,
    py_strings: Vec<Py<PyString>>,
}

impl StringFactory {
    fn new() -> Self {
        StringFactory {
            string_interner: StringInterner::new(),
            py_strings: Vec::new(),
        }
    }

    // either create a new string or reuse an existing one if we've already seen this string before
    fn create_or_reuse(&mut self, py: Python<'_>, x: &str) -> Py<PyString> {
        let sym = self.create_or_lookup_symbol(py, x);
        self.reuse(py, sym)
    }

    // reuse a string we have in our symbol table
    fn reuse(&mut self, py: Python<'_>, sym: SymbolU32) -> Py<PyString> {
        self.py_strings[sym.to_usize()].clone_ref(py)
    }

    fn create_or_lookup_symbol(&mut self, py: Python<'_>, x: &str) -> SymbolU32 {
        use Ordering::*;

        let sym = self.string_interner.get_or_intern(x);
        let i_sym = sym.to_usize();

        // allocate a new PyString if we don't already have it
        match i_sym.cmp(&self.py_strings.len()) {
            Less => (),
            Equal => {
                self.py_strings.push(PyString::new(py, x).into());
            }
            Greater => {
                // impossible
                panic!(
                    "unexpected symbol {:?} with index {} from string_interner with vec len {}",
                    sym,
                    i_sym,
                    self.py_strings.len()
                );
            }
        }

        sym
    }
}

struct DateFactory {
    cached_date: Option<Py<PyDate>>,
}

impl DateFactory {
    pub(crate) fn new() -> Self {
        DateFactory { cached_date: None }
    }

    // reuse the cached date if it matches, otherwise discard and create a new one.
    fn create_or_reuse(&mut self, py: Python<'_>, x: &Date) -> Py<PyDate> {
        let cached_date = self.cached_date.take();

        self.cached_date = match cached_date {
            Some(cached_date) => {
                let py_date = cached_date.bind(py);
                if py_date.get_year() == x.year()
                    && py_date.get_month() == x.month() as u8
                    && py_date.get_day() == x.day()
                {
                    Some(cached_date)
                } else {
                    Some(create_date(py, x))
                }
            }
            None => Some(create_date(py, x)),
        };

        self.cached_date.as_ref().unwrap().clone_ref(py)
    }
}

fn create_date(py: Python<'_>, x: &Date) -> Py<PyDate> {
    PyDate::new(py, x.year(), x.month() as u8, x.day())
        .unwrap()
        .into()
}
