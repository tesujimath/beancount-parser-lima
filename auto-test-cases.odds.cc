TEST(TestParserComplete, Empty1) {
  ExpectParse("", R"(
  )");
}

TEST(TestParserComplete, ExtraWhitespaceTransaction) {
  ExpectParse(absl::StrCat(
    "2013-05-18 * \"Nice dinner at Mermaid Inn\"\n",
    "  Expenses:Restaurant         100 USD\n",
    "  Assets:US:Cash\n",
    "  \n",
    ";; End of file"), R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", { .no_dedent = true });
}

TEST(TestParserComplete, ExtraWhitespaceComment) {
  ExpectParse(absl::StrCat(
    "2013-05-18 * \"Nice dinner at Mermaid Inn\"\n",
    "  Expenses:Restaurant         100 USD\n",
    "  Assets:US:Cash\n",
    "  ;;"), R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", { .no_dedent = true });
}

TEST(TestParserComplete, ExtraWhitespaceCommentIndented) {
  ExpectParse(absl::StrCat(
    "2013-05-18 * \"Nice dinner at Mermaid Inn\"\n",
    "  Expenses:Restaurant         100 USD\n",
    "  Assets:US:Cash\n",
    "    ;;"), R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", { .no_dedent = true });
}

TEST(TestParserComplete, IndentEOF) {
  ExpectParse("\t", R"(
  )", { .no_dedent = true });
}

TEST(TestParserComplete, CommentEOF) {
  ExpectParse("; comment", R"(
  )", { .no_dedent = true });
}

TEST(TestSyntaxErrors, NoFinalNewline) {
  // Note: We would have to explicitly have to simulate and dedents here for
  // this to work and this is surprisingly tricky and error prone. See
  // {fab24459d79d} Prefer not to handle.
  ExpectParse(absl::StrCat(
    "2014-11-02 *\n",
    "  Assets:Something   1 USD\n",
    "  Assets:Other      -1 USD"),
  R"(
    errors {
      message: "Syntax error, unexpected DEDENT, expecting EOL or ATAT or AT"
    }
  )", { .no_dedent = true });
}

TEST(TestTransactions, BlankLineWithSpacesNotAllowed) {
  // Note: This resulted in an error in v2. The difference is due to how
  // processing of indents is improved in v3.
  ExpectParse(absl::StrCat(
            "2014-04-20 * \"Busted!\"\n",
            "  Assets:Checking         100 USD\n",
            "  \n",  // This is not allowed.
            "  Assets:Checking         -99 USD\n"),
  R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      transaction {
        flag: "*"
        narration: "Busted!"
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "-99" } currency: "USD" } }
        }
      }
    }
  )", { .no_dedent = true });
}
