build:
    cargo build

test:
    cargo test

build-image := "ghcr.io/pyo3/maturin"

[working-directory: 'beancount-parser-lima-python']
build-python-wheel:
    docker run --rm -v $(pwd)/..:/io -w /io/beancount-parser-lima-python {{build-image}} build --release

upload-testpypi:
    twine upload -r testpypi "target/wheels/$(ls -t target/wheels | head -1)"

upload-pypi:
    twine upload "target/wheels/$(ls -t target/wheels | head -1)"

clean:
    cargo clean
    docker rm {{build-image}}

