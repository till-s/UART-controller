# UART Controller

     Till Straumann <till.straumann@alumni.tu-berlin.de>, 2024.

Simple UART controller and bit-rate generator. The core
may run at a relatively high frequency (e.g., 60MHz when
attached to USB) which allows the rate generator to produce
any desired bit rate. The rate-generator supports fractional
rate-dividers so that errors do not accumulate over multiple
bits.

The design is very simple: the receiver uses just a single
sample point at the center of a bit-cell.

The rate generator is re-synchronized to the main clock
at every start bit.

## License

The UART controller is released under the [European-Union Public
License](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12).

The EUPL permits including/merging/distributing the licensed code with
products released under some other licenses, e.g., the GPL variants.

I'm also open to use a different license.
