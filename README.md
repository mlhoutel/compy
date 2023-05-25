# Compy

Compy is a program that apply transformations on Python programs. This tool is currently in development and is considered a preview, which means that it may be unstable and doesn't support all Python3 language features.

## Transformations

Here are some of the transformations that Compy can handle, along with their status. If you have idea for new transformations, don't hesitate to add and issue. If you would like to contribute, you can fork the repository and create a pull request with your changes. Please make sure to write tests for any new features or bug fixes, and to run `cargo test` and `cargo fmt` before submitting your pull request.

### :white_check_mark: No transformations

### :construction: To One-liner

| Feature                                       | Status                  |
| --------------------------------------------- | ----------------------- |
| Variables and data types                      | :white_check_mark: Done |
| Control structures (if/else, for/while loops) | :white_check_mark: Done |
| Functions and lambdas                         | :white_check_mark: Done |
| Classes and objects                           | :white_check_mark: Done |
| Annotations                                   | :white_check_mark: Done |
| List, tuple, and dictionary manipulation      | :white_check_mark: Done |
| Modules and packages                          | :white_check_mark: Done |
| Decorators                                    | :white_check_mark: Done |
| Built-in functions and modules                | :white_check_mark: Done |
| Generators and iterators                      | :white_check_mark: Done |
| Globals                                       | :white_check_mark: Done |
| Delete                                        | :white_check_mark: Done |
| Exception handling                            | :construction: In work  |
| Context managers                              | :construction: In work  |
| Async                                         | :x: Not supported yet   |
| Type hints and static typing                  | :x: Not supported yet   |

## Getting started

```sh
git clone https://github.com/mlhoutel/compy.git && cd compy
```

### Library development

```sh
cargo test
cargo build
```

### Demo website

```sh
cd docs
wasm-pack build --target web --out-dir static
npm i && npm run start
```

Once the server is running, you can open your web browser and navigate to http://localhost:8000 to see the demo website in action.
