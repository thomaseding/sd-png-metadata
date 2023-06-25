# sd-png-metadata
PNG metadata tool for Automatic1111 SD images.

---

```
Usage:
 sd-png-metadata <options>

Options
 --help               Show this help text.
 --input-image        Input image file path. Required.
 --output-image       Output image file path.
 --output-caption     Output caption file path.
 --print              Print the input image metadata to stdout.
 --overwrite-input    Overwrite the input image with the metadata.
 --force              Disable all overwrite checks.
 --edit <key> <value> Edit the metadata.

Supported keys:
  "Positive prompt"
  "Negative prompt"
  "Steps"
  "Sampler"
  "CFG scale"
  "Seed"
  "Size"
  "Model hash"
  "Model"
  "Lora hashes"
  "Version"
  "Template"
  "Negative Template"
```
