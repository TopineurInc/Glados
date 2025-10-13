# Topineur VS Code Extension

This extension adds basic editor support for the Topineur language:

* file association for ``.top`` sources;
* syntax highlighting via a TextMate grammar;
* comment toggling and auto-closing pairs defined through a language configuration.

## Installing Locally

1. Run ``npm install`` in this directory if you plan to package the extension later (optional).
2. From the repository root, execute:

   ```bash
   cd vscode/topineur
   npm install --global vsce   # only once; optional if you already use vsce
   vsce package
   ```

   This produces a ``topineur-language-0.1.0.vsix`` file.
3. Open VS Code and install the package via ``Extensions → ... → Install from VSIX``.

Alternatively, for quick iteration you can use ``F5`` inside VS Code to launch a development instance with the extension enabled.

## Development Notes

* Grammar rules live in ``syntaxes/topineur.tmLanguage.json``.  Update the repository to refine highlighting.
* ``language-configuration.json`` controls bracket/quote auto-closing and comment styles.
* Keep ``package.json`` versioned alongside any meaningful changes so ``vsce`` can emit an updated VSIX.
