## About the project

This is the source code for the [Functor Network](https://functor.network) - a blog platform for mathematicians. The term "mathematician" here is used in a broad sense, encompassing people studying math at any level, as well as professionals in related fields. While we provide special support for mathematicians, anyone can register a blog site on our platform and express themselves on a variety of topics, whether they're related to math or not.

## Features


In a word, every feature on our platform is thoughtfully designed for mathematicians.

- **Effortless Typesetting.** Typesetting math on the web can be exhausting and time-consuming. On our platform, you focus on the content—we’ll take care of the typesetting.

- **Powered by a Real TeX Engine.** We might be the only blogging platform that renders mathematical content using a full TeX Live system. Enjoy complete LaTeX compatibility—import packages, use math environments, or even paste existing LaTeX documents directly.  

- **Flexible Writing Formats.** Write in *Pure LaTeX* or *Markdown with LaTeX*—your choice. Either way, you retain powerful features like cross-referencing, bibliographies, and theorem environments.  

- **PDF Export.** Need an offline copy? Download any post as a beautifully typeset PDF with one click.  

- **Minimalist & Focused.** No clutter. No distractions. Just a clean, efficient, and secure space for your mathematical thoughts.  


## Comparison

We make a comparison with other blog platform to clarify our features:

| Feature                          | Our Platform                                              | Other Platforms                                            |
|----------------------------------|------------------------------------------------------------|------------------------------------------------------------|
| Write posts directly in LaTeX    | Supported                                                  | Not supported                                              |
| Math formula rendering           | Built-in support                                           | Not supported or requires add-ons                         |
| Preserve LaTeX code              | Yes                                                        | May corrupt backslashes or encode symbols like `<`, `&`, etc. |
| LaTeX packages                   | Supported — different packages can be used per post        | Not supported                                              |
| Automated numbering and referencing | Supported                                               | Not supported or only partially supported                 |
| Custom LaTeX commands            | Supported                                                  | Possibly supported                                         |
| Theorem-like environments        | Supported                                                  | Not supported                                              |
| Bibliography                     | Supported                                                  | Possibly supported                                         |
| Markdown with math features      | Supported                                                  | Possibly supported but limited                             |
| PDF export                       | Supported                                                  | Not supported                                              |
| Design philosophy                | Clean and minimal — intentionally abstract                 | Often cluttered with unnecessary features                  |
| Editor                           | Plain text editor with live preview — focuses on content   | WYSIWYG editor — may be problematic for math content       |


## Development

The project is developed on the [Yesod](https://www.yesodweb.com/) web framework. To build the project, you can follow the framework's instructions. 