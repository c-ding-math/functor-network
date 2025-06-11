## About the project

This is the source code for the [Functor Network](https://functor.network) - a blog platform for mathematicians. The term "mathematician" here is used in a broad sense, encompassing people studying math at any level, as well as professionals in related fields. While we provide special support for mathematicians, anyone can register a blog site on our platform and express themselves on a variety of topics, whether they're related to math or not.

## Features


In a word, every feature on our platform is thoughtfully designed for mathematicians.

- **Effortless Typesetting.** Typesetting math on the web can be exhausting and time-consuming. On our platform, you focus on the content—we’ll take care of the typesetting.

- **Powered by a Real TeX Engine.** We might be the only blogging platform that renders mathematical content using a full TeX Live system. Enjoy complete LaTeX compatibility—import packages, use math environments, or even paste existing LaTeX documents directly.  

- **Flexible Writing Formats.** Write in *Pure LaTeX* or *Markdown with LaTeX*—your choice. Either way, you retain powerful features like cross-referencing, bibliographies, and theorem environments.  

- **PDF Export.** Need an offline copy? Download any post as a beautifully typeset PDF with one click.  

- **Minimalist & Focused.** No clutter. No distractions. Just a clean, efficient, and secure space for your mathematical thoughts.  

Other small but useful features, including syntax highlighting for Lean, support for social sharing platforms commonly used by mathematicians, and so on.


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
| Syntax highlight for Lean        | Supported                                                  | Possibly supported                                              |
| Design philosophy                | Clean and minimal — intentionally abstract                 | Often cluttered with unnecessary features                  |
| Editor                           | Plain text editor with live preview — focuses on content   | WYSIWYG editor — may be problematic for math content       |


## Credits

**Functor Network** is developed by [Chun Ding](https://github.com/c-ding-math), © 2023-2025 Chun Ding, licensed under the [MIT License](https://opensource.org/licenses/MIT).  
It is built on the [Yesod](https://www.yesodweb.com/) web framework, © 2012–2017 Michael Snoyman, also under the [MIT License](https://opensource.org/licenses/MIT).

This project includes the following third-party resources:

- `static/js/jquery.min.js` — [jQuery](https://jquery.com/), © 2005–2025 jQuery Foundation, [MIT License](https://opensource.org/licenses/MIT)  
- `static/css/bootstrap.min.css` and `static/js/bootstrap.min.js` — [Bootstrap](https://getbootstrap.com/), © 2011–2025 Twitter, Inc., [MIT License](https://opensource.org/licenses/MIT)  
- SVG icons from [Bootstrap](https://getbootstrap.com/), © 2011–2025 Twitter, Inc., [MIT License](https://opensource.org/licenses/MIT)  
- Syntax definition for Lean language highlighting — © 2025 [Hagb (Junyu Guo)](https://github.com/Hagb), [MIT License](https://opensource.org/licenses/MIT)  
- `static/editor` — [Ace Editor](https://ace.c9.io/), © 2010 Ajax.org B.V., [License](static/editor/LICENSE)
- `static/js/js.cookie.min.js` - [js-cookie](https://github.com/js-cookie/js-cookie), © 2018 Klaus Hartl, Fagner Brack, GitHub Contributors, [MIT License](https://opensource.org/licenses/MIT)
- `static/js/qrcode.min.js` - [QRCode.js](https://github.com/davidshimjs/qrcodejs), © 2012 davidshimjs, [MIT License](https://opensource.org/licenses/MIT)
