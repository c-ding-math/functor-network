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

|           | Our platform  | Other platform |
|-----------|---------------|----------------|
| writing post directly in latex | supports | doesn't support |
| math-featured markdown | supports | maybe supports, but has limited features on math |
| math formula rendering | has built-in support | doesn't support or needs add-ons to support |
| keep latex code unchanged | yes | may eat backslashes, encode `<`, `&` and other math symbols |
| latex packages | supports. You can use different latex packages for each post separately | doesn't support |
| automated numbering and referencing | supports. You can use `\label{}` and `\ref{}` for this. | doesn't support or partially supports |
| custom latex commands | supports. You can define your own latex command for each post. | maybe supports |
| theorem-like environments | supports | doesn't support |
| bibliography | supports | maybe supports |
| clean and neat | Yes. Our platform is designed with minimalism in mind. It even looks a bit abstract. | may have many unnecessary functions |
| editor | provides a plain text editor with a preview window. You can focus more on the content itself rather than its appearance. | provides a WYSIWYG editor. This kind of editor may be annoying when handling math content. |

## Development

The project is developed on the [Yesod](https://www.yesodweb.com/) web framework. To build the project, you can follow the framework's instructions. 