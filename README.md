# sd-png-metadata
PNG metadata tool for Automatic1111 SD images.

---

```
Usage:
 sd-png-metadata <options>

Options
  --help                Show this help text.
  --input-image         Input image file path. Required.
  --output-image        Output image file path.
  --output-caption      Output caption file path.
  --log-file            Log file path.
  --pattern-file        Pattern file path for caption output.
  --print               Print the input image metadata to stdout.
  --print-key <key>     Print the input image metadata key to stdout.
  --overwrite-input     Overwrite the input image with the metadata.
  --batch               Batch mode. Specified files must be directories.
  --force               Disable all overwrite checks.
  --create-missing-dirs Create any missing directories.
  --edit <key> <value>  Edit the metadata.
  --no-prompt-weights   Removes weights from metadata prompts.

Pattern file format:
  some text before subject * and some after
  * some text after the subject
  some text before the subject *
  as many pattern lines * in the pattern file as you want

Supported keys:
  "Positive prompt"
  "Negative prompt"
  "Steps"
  "Sampler"
  "CFG scale"
  "Seed"
  "Face restoration"
  "Size"
  "Model hash"
  "Model"
  "Clip skip"
  "CFG Rescale phi"
  "Lora hashes"
  "Version"
  "Template"
  "Negative Template"
  "ControlNet 0"
```
