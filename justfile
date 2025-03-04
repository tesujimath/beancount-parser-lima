build:
    cargo build

test:
    cargo test

build-image := "maturin-protoc:1"
[working-directory: 'beancount-parser-lima-python/docker']
build-container:
    docker build . -t {{build-image}}

[working-directory: 'beancount-parser-lima-python']
build-python-wheel: build-container
    docker run --rm -v $(pwd)/..:/io -w /io/beancount-parser-lima-python {{build-image}} build --release

upload-testpypi:
    twine upload -r testpypi "target/wheels/$(ls -t target/wheels | head -1)"

upload-pypi:
    twine upload "target/wheels/$(ls -t target/wheels | head -1)"

clean:
    cargo clean
    docker rm {{build-image}}

