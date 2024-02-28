fn create_proto_posting() -> Posting {
    Posting {
        location: None,
        meta: None,
        date: None,
        flag: None,
        account: None,
        position: None,
        price: None,
        spec: None,
    }
}

mod beancount;
use beancount::Posting;
