{
  description = "A Rust development environment flake.";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ (import rust-overlay) ];
          pkgs = import nixpkgs {
            inherit system overlays;
          };
          # cargo-nightly based on https://github.com/oxalica/rust-overlay/issues/82
          nightly = pkgs.rust-bin.selectLatestNightlyWith (t: t.default);
          cargo-nightly = pkgs.writeShellScriptBin "cargo-nightly" ''
              export RUSTC="${nightly}/bin/rustc";
              exec "${nightly}/bin/cargo" "$@"
          '';
        in
          with pkgs;
          {
            devShells.default = mkShell {
              nativeBuildInputs = [
                beancount

                # build dependencies
                cargo-flamegraph
                cargo-modules
                cargo-nightly
                cargo-udeps
                gcc
                gdb
                rust-analyzer
                rust-bin.stable.latest.default
                rustfmt

                # for protoc, used for parser test schema
                protobuf

                # for Python bindings
                maturin
                python3
                zig
                twine

                # Python devtools
                pyright
              ];
            };
          }
      );
}
