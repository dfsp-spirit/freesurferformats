# Add a label tabel from an annotation to a GIFTI XML tree.

Computes the LabelTable XML node for the given annotation and adds it to
the XML tree.

## Usage

``` r
giftixml_add_labeltable_from_annot(xmltree, annot)
```

## Arguments

- xmltree:

  an XML tree from xml2, typically the return value from
  [`gifti_xml`](https://dfsp-spirit.github.io/freesurferformats/reference/gifti_xml.md).

- annot:

  an fs.annotation, the included data will be used to compute the
  LabelTable node

## Value

XML tree from xml2, the modified tree with the LabelTable added below
the root node.
