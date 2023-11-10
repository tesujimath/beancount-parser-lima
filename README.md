# beancount-parser

A zero-copy parser for Beancount in Rust.

It is intended to be a complete implementation of the Beancount file format, except for those parts which are deprecated and other features as documented here (in a list which may not be comprehensive).

Currently under active development.  APIs are subject to change, but I hope not majorly.

(Note that zero-copy support from Chumsky is currently available only in alpha releases.)

## Features

- fast, thanks to [Logos](https://docs.rs/logos/latest/logos/) and [Chumsky](https://docs.rs/chumsky/latest/chumsky/) zero copy

- beautiful error messages, thanks to [Ariadne](https://docs.rs/ariadne/latest/ariadne/)

- interface for applications to also report beautiful errors in their original context, as in the example below

- focus on conceptual clarity of application domain objects mapped to Rust types

<img src="https://raw.githubusercontent.com/tesujimath/beancount-parser/master/examples/images/beancount-parser-balancing-errors.png" alt="Example application error messages"/>

## Roadmap

- create Python bindings, so that this could be a drop-in replacement for the existing Beancount parser (which is not to say it will necessarily become that!)

- improve API in the light of experience, i.e. when it gets some use 😅

- address mistakes, misunderstandings, and edge-cases in the initial implementation as they are discovered

## Uncertainties / TODOs

Yeah, Beancount is complicated, and I may have made some mistakes here.  Current list of uncertainties, which is certainly not comprehensive.

- metadata tags/links for a directive get folded in with those in the directive header line

## Unsupported

This is an incomplete list of what is currently unsupported.

- plugins

### Unsupported Options

- `allow_pipe_separator`
- `allow_deprecated_none_for_tags_and_links`
- `default_tolerance`
- `experiment_explicit_tolerances`
- `insert_pythonpath`
- `plugin`
- `plugin_processing_mode`
- `tolerance`
- `use_legacy_fixed_tolerances`

Also, unary options are not supported.

## License

Licensed under either of

 * Apache License, Version 2.0
   [LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   [LICENSE-MIT](http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
