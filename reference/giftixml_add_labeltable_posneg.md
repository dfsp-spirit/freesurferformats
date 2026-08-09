# Add a standard label tabel for to a GIFTI XML tree.

This label table is suitable for labels in the FreeSurfer sense, i.e.,
if only a positive label (1) and a negative label (0) exist in the label
data.

## Usage

``` r
giftixml_add_labeltable_posneg(xmltree)
```

## Arguments

- xmltree:

  an XML tree from xml2, typically the return value from
  [`gifti_xml`](https://dfsp-spirit.github.io/freesurferformats/reference/gifti_xml.md).

## Value

XML tree from xml2, the modified tree with the LabelTable added below
the root node.
