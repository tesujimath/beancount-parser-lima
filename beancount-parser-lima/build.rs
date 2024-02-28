use std::{
    env::{self, VarError},
    path::{Path, PathBuf},
};

fn clone_beancount() -> Option<PathBuf> {
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
            println!("cargo:rerun-if-changed=.cargo/config.toml");

            let out_dir: PathBuf = env::var("OUT_DIR").unwrap().into();
            let beancount_dir = out_dir.join(format!("beancount-{}", version));

            if Path::exists(&beancount_dir) {
                Some(beancount_dir)
            } else {
                let beancount_dir_string = beancount_dir.to_string_lossy();
                let git_args = [
                    "clone",
                    "--filter=blob:none",
                    "--branch",
                    version.as_ref(),
                    "https://github.com/beancount/beancount.git",
                    beancount_dir_string.as_ref(),
                ];
                match std::process::Command::new("git").args(git_args).output() {
                    Ok(output) => {
                        if output.status.success() {
                            Some(beancount_dir)
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
    if let Some(beancount_dir) = clone_beancount() {
        let proto_path = beancount_dir.join("beancount/ccore/data.proto");
        prost_build::compile_protos(&[proto_path], &[beancount_dir])?;
    }

    Ok(())
}
