use std::{
    env::{self, VarError},
    path::{Path, PathBuf},
};

fn fetch_beancount_proto() -> Option<PathBuf> {
    use VarError::*;

    match env::var("BEANCOUNT_PROTO_VERSION") {
        Err(NotPresent) => {
            println!("cargo:warning=missing environment variable BEANCOUNT_PROTO_VERSION for git clone in build script");

            None
        }
        Err(NotUnicode(_)) => {
            println!("cargo:warning=bad environment variable BEANCOUNT_PROTO_VERSION for git clone in build script");

            None
        }
        Ok(version) => {
            println!("cargo:rerun-if-changed=../.cargo/config.toml");

            let out_dir: PathBuf = env::var("OUT_DIR").unwrap().into();
            let beancount_repo_dir: PathBuf = format!("beancount-{}", version).into();
            let beancount_repo_path = out_dir.join(beancount_repo_dir.as_path());

            if Path::exists(&beancount_repo_path) {
                Some(beancount_repo_path)
            } else {
                let beancount_repo_path_str = beancount_repo_path.to_string_lossy();
                let git_args = [
                    "clone",
                    "--filter=blob:none",
                    "--branch",
                    version.as_ref(),
                    "https://github.com/beancount/beancount.git",
                    beancount_repo_path_str.as_ref(),
                ];
                match std::process::Command::new("git").args(git_args).output() {
                    Ok(output) => {
                        if output.status.success() {
                            Some(beancount_repo_path)
                        } else {
                            println!(
                                "cargo:warning=git {:?} failed: {}",
                                &git_args, output.status
                            );

                            None
                        }
                    }
                    Err(e) => {
                        println!("cargo:warning=git clone failed: {}", e);

                        None
                    }
                }
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    if let Some(beancount_repo_path) = fetch_beancount_proto() {
        protobuf_codegen::Codegen::new()
            .protoc()
            .include(beancount_repo_path.as_path())
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
                .map(|input| beancount_repo_path.join(input)),
            )
            .cargo_out_dir("proto")
            .run_from_script();
    }

    Ok(())
}
