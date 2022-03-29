# QOI

QOI is a Common Lisp library for reading and writing [QOI](https://qoiformat.org/) (Quite OK Image Format) files.

## decoding/reading

`decode` takes an input stream of octets and decodes it to an `image`. `image` is an 1D array of size `width * height * channels` (channels = 3 for RGB file and = 4 for RGBA file).

To decode a `qoi` file simply:
```lisp
(with-open-file (stream #p"./dice.qoi" :element-type '(unsigned-byte 8))
  (decode stream))
```

`decode` returns multiple values: `(values image width height channels colorspace)`.

## encoding/writing

`encode` takes an binary output stream and an image array then encodes the image and writes it to that stream.

To encode and save an `image` to `output-file` smiply:
```lisp
(with-open-file (stream output-file
                        :direction :output
                        :if-exists :supersede
                        :element-type '(unsigned-byte 8))
  (encode stream image width height channels colorspace))
```

## License

MIT
