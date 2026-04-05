use std::{env, path::PathBuf};

fn autogen_beancount_proto() {
    let cargo_manifest_dir: PathBuf = env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let proto_dir = cargo_manifest_dir.join("protobuf");

    protobuf_codegen::Codegen::new()
        .pure()
        .include(proto_dir.as_path())
        .inputs(
            [
                "beancount/cparser/options.proto",
                "beancount/cparser/inter.proto",
                "beancount/cparser/ledger.proto",
                "beancount/ccore/date.proto",
                "beancount/ccore/precision.proto",
                "beancount/ccore/number.proto",
                "beancount/ccore/data.proto",
            ]
            .iter()
            .map(|input| proto_dir.join(input)),
        )
        .cargo_out_dir("proto")
        .run_from_script();
}
fn main() -> std::io::Result<()> {
    let cargo_manifest_dir: PathBuf = env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let proto_beancount_dir = cargo_manifest_dir.join("protobuf/beancount");
    let proto_subdirs = ["cparser", "ccore"];

    println!("cargo:rerun-if-changed=build.rs");
    for proto_subdir in &proto_subdirs {
        println!(
            "cargo:rerun-if-changed={}",
            proto_beancount_dir.join(proto_subdir).display()
        );
    }

    autogen_beancount_proto();

    Ok(())
}
