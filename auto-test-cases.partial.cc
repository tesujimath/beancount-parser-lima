TEST(TestCurrencies, DifferentCostAndPriceCurrency) {
  ExpectParse(R"(
    2018-03-21 * "Convert MR to KrisFlyer"
      Assets:Test                -100 MR {0.0075 USD} @ 1 KRISFLYER
      Assets:Krisflyer            100 KRISFLYER
  )", R"(
    directives {
      # ..
    }
    errors {
      message: "Cost and price currencies must match: USD != KRISFLYER"
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalCost) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{2,000 USD}}
      Assets:Investments:Cash  -20000 USD

    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{2000 USD, 2014-02-25}}
      Assets:Investments:Cash  -20000 USD

    2013-06-01 * ""
      Assets:Investments:MSFT      -10 MSFT {{2,000 USD}}
      Assets:Investments:Cash    20000 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost { total { number { exact: "2000" } } currency: "USD" }
          }
        }
        postings {
          spec { units { number { exact: "-20000" } currency: "USD" } }
        }
      }
    }
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost {
              total { number { exact: "2000" } } currency: "USD"
              date { year: 2014 month: 2 day: 25 }
            }
          }
        }
        postings {
          spec {
            units { number { exact: "-20000" } currency: "USD" }
          }
        }
      }
    }
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            cost { total { number { exact: "2000" } } currency: "USD" }
          }
        }
        postings {
          spec {
            units { number { exact: "20000" } currency: "USD" }
          }
        }
      }
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalCostInvalid) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{100 # 2,000 USD}}
      Assets:Investments:Cash  -20000 USD
  )", R"(
    directives {
      # ...
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost { total { number { exact: "2000" } } currency: "USD" }
          }
        }
        postings {
          spec { units { number { exact: "-20000" } currency: "USD" } }
        }
      }
    }
    errors {
      message: "Per-unit cost may not be specified using total cost syntax: \'per_unit {\n  number {\n    exact: \"100\"\n  }\n}\ntotal {\n  number {\n    exact: \"2000\"\n  }\n}\ncurrency: \"USD\"\n\'; ignoring per-unit cost"
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalCostNegative) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      -10 MSFT {{-200.00 USD}}
      Assets:Investments:Cash   200.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            cost { total { number { exact: "-200.00" } } currency: "USD" }
          }
        }
        postings {
          # ...
        }
      }
    }
  )", { .partial = true });
  // Should produce no errors.
  // Note: This error is caught only at booking time.
}

TEST(TestTotalsAndSigns, PriceNegative) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      -10 MSFT @ -200.00 USD
      Assets:Investments:Cash  2000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            price { number { exact: "200.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "2000.00" } currency: "USD" }
          }
        }
      }
    }
    errors {
      message: 'Negative prices are not allowed (see http://furius.ca/beancount/doc/bug-negative-prices for workaround)'
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalPricePositive) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT        10 MSFT @@ 2000.00 USD
      Assets:Investments:Cash  -2000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000.00" } currency: "USD" is_total: true }
          }
        }
        postings {
          # ...
        }
      }
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalPriceNegative) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT       -10 MSFT @@ 2000.00 USD
      Assets:Investments:Cash  20000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            price { number { exact: "2000.00" } currency: "USD" is_total: true }
          }
        }
        postings {
          # ...
        }
      }
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalPriceInverted) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT         10 MSFT @@ -2000.00 USD
      Assets:Investments:Cash   20000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000.00" } currency: "USD" is_total: true }
          }
        }
        postings {
          # ...
        }
      }
    }
    errors {
      message: 'Negative prices are not allowed (see http://furius.ca/beancount/doc/bug-negative-prices for workaround)'
    }
  )", { .partial = true });
}
