TEST(TestParserInclude, ParseNonExisting) {
  auto ledger = parser::ParseFile("/some/bullshit/filename.beancount");
  auto ledger_proto = LedgerToProto(*ledger);
  for (auto& error : *ledger_proto->mutable_errors()) {
    error.clear_location();
  }
  EXPECT_TRUE(EqualsMessages(*ledger_proto, R"(
    errors {
      message: "An IO error has occurred."
    }
  )", false));
}

TEST(TestParserInclude, IncludeAbsolute) {
  ExpectParse(R"(
    include "/some/absolute/filename.beancount"
  )", R"(
    info {
      include: "/some/absolute/filename.beancount"
    }
  )");
}

TEST(TestParserInclude, IncludeRelativeFromString) {
  ExpectParse(R"(
    include "some/relative/filename.beancount"
  )", R"(
    info {
      include: "some/relative/filename.beancount"
    }
  )");
}
