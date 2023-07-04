
## Installation

First, you have to install Rust and Cargo (the build tool for Rust).
Follow the instructions at the Rust website (https://www.rust-lang.org/tools/install).
Make sure you add the `~/.cargo/bin` directory to your `Path`.

Then download this repository somewhere onto your computer.
Unzip it and navigate to it, so that the directory containing `Cargo.toml` is your current working directory.
Run `cargo run`. This should automatically install dependencies, build, and run the project.

## Usage

For an example usage, download [this](https://drive.google.com/file/d/1i0fIGKroalNtzo8_BHj2HQBH4KFGvSy-/view?usp=sharing) launch file, then select it as the source and `example/packet.h` as the packet format.

Left-click+Drag in the plot window to box zoom. To reset the zoom, double-click in the plot window.

## Limitations

Loading in all the rows from the launch file into the graph makes the application laggy.
To mitigate this, increase the sampling in the plot tab to reduce the number of data points plotted.
