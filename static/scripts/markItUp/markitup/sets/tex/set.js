// ----------------------------------------------------------------------------
// markItUp!
// ----------------------------------------------------------------------------
// Copyright (C) 2008 Jay Salvat
// http://markitup.jaysalvat.com/
// ----------------------------------------------------------------------------

editorSettings = {
    nameSpace:          'latex', // Useful to prevent multi-instances CSS conflict
	onCtrlEnter:        {keepDefault:false, call:'preview'}, //not working
    markupSet: [
		{name:'Text', className:'', dropMenu:[
			{name:'Italic', key:"I", openWith:'\\emph{', closeWith:'}',placeHolder:'emphasized text'},
            {name:'Bold', key:"B", openWith:'\\textbf{', closeWith:'}',placeHolder:'strong text'},
            {name:'Quote', key:"Q", placeHolder:'Blockquote',
            openWith:'\\begin{quotation}\n',closeWith:'\n\\end{quotation}',
            openBlockWith:function(markItUp){return markIt.latexBlock(markItUp).openBlockWith;},
            closeBlockWith:function(markItUp){return markIt.latexBlock(markItUp).closeBlockWith;},
		    },
			{
				name: 'Heading', key: "H", className: 'Heading fas fa-heading', placeHolder: 'Your title here...',
				openWith:'\\section{',closeWith:'}',multiline:true,
				openBlockWith:function(markItUp){return markIt.latexBlock(markItUp).openBlockWith;},
				closeBlockWith:function(markItUp){return markIt.latexBlock(markItUp).closeBlockWith;},
			},
			{name:'Bulleted list', key:'U', openWith:'  \\item ', className:'bulleted-list, fas fa-list-ul',placeHolder:'List item',
				openBlockWith:function(markItUp){return markIt.latexBlock(markItUp).openBlockWith + '\\begin{itemize}\n';},
				closeBlockWith:function(markItUp){return '\n\\end{itemize}'+ markIt.latexBlock(markItUp).closeBlockWith;},
				multiline:true,
			},
			{name:'Numbered list',key:'O', className:'numeric-list fas fa-list-ol',placeHolder:'List item', openWith:'  \\item ',
				openBlockWith:function(markItUp){return markIt.latexBlock(markItUp).openBlockWith + '\\begin{enumerate}\n';},
				closeBlockWith:function(markItUp){return '\n\\end{enumerate}'+ markIt.latexBlock(markItUp).closeBlockWith;},
				multiline:true,
			},
            {name:'Link', key:"L",className:"fas fa-link necessary",
			replaceWith:function(markItUp){markIt.latexLink(markItUp);return false;},
		    },
			{name:'Image', key:"G",className:"fas fa-image necessary",
			replaceWith:function(markItUp){markIt.latexImage(markItUp);return false;},
			},
			/*{name:'Highlight block', key:"H",
			openWith:':::{.mark}\n',closeWith:'\n:::',placeHolder:"Your content goes here...",
            openBlockWith:function(markItUp){return markIt.latexBlock(markItUp).openBlockWith;},
            closeBlockWith:function(markItUp){return markIt.latexBlock(markItUp).closeBlockWith;},
			},*/
        ]},
        {name:'Math', className:'', dropMenu:[
			{name:'Inline math', key:"1", openWith:'$', closeWith:'$',replaceWith: function(markItUp){return (markItUp.selection).trim();},placeHolder:'latex code'},
            {name:'Display math', key:"2", openWith:'$$', closeWith:'$$',placeHolder:'latex code'},
			{name:'Numbered equation', key:"3", 
			replaceWith:function(markItUp){markIt.latexNumberedEquation(markItUp);return false;},
			},
			{name:'Aligned equations', key:"4", 
			openBlockWith:function(markItUp){return markIt.latexBlock(markItUp).openBlockWith + '\\begin{align*}\n';},
			closeBlockWith:function(markItUp){return '\n\\end{align*}'+ markIt.latexBlock(markItUp).closeBlockWith;},
			replaceWith:function(markItUp){
				if(markItUp.selection==""){	
					return "";				
				}else{
					var lines=markItUp.selection.split(/\n/);
					for (var i = 0; i < lines.length; i++) {			
						if(i==lines.length-1){
							lines[i]='& ' + lines[i]
						}else{
							lines[i]='& ' + lines[i] + '\\\\'
						}
					}
					return lines.join('\n');
				}
			},
			placeHolder:'& first line\\\\\n& second line\\\\\n& ...',
			},
			{name:'Equation reference', key:"5", openWith:'\\eqref{', closeWith:'}',placeHolder:'equation label'},
        ]},
		{name:'Meta', dropMenu:[
			{
				name:'Latex preamble', key:"8", replaceWith:function(markItUp){markIt.latexPreamble(markItUp);return false;},
			},
			{name:'Bibliography', key: "9", className:'',replaceWith:function(markItUp){markIt.latexCitation(markItUp);return false;},},
		]},
        
        {name:'Help', className:'', dropMenu:[
			{name:'Syntax help',replaceWith:function(markItUp){markIt.latexSyntaxHelp(markItUp);return false;},},	
			{name:'Editor help',replaceWith:function(markItUp){markIt.latexEditorHelp(markItUp);return false;},},
        ]},

        {name:'Preview', className:"preview", dropMenu:[
			{name:"Update preview", key:'0', call:'preview'},
			//{name:"Shrink", key:"", replaceWith:function(markItUp){markIt.editorShrink(markItUp);return false;},className:"normal-size hidden"},
			{name:"Side by side", key:'M', className:'toggle', call:'preview',replaceWith:function(markItUp){markIt.editorToggle(markItUp);return false;}},
		]},
      
    ]
}
markIt = {
	counter:0,
	
	editorToggle: function(markItUp) {
		$(".markItUpContainer").toggleClass("full-screen");
		if($(".markItUpContainer").hasClass("full-screen")) {
			$(".markItUpContainer .toggle a").text("Normal size");
		} else {
			$(".markItUpContainer .toggle a").text("Side by side");
		}
		return false;
	},
	latexSyntaxHelp: function(markItUp) {
		window.open('https://functor.network/help/syntax?format=tex');
		return false;
	},
	latexEditorHelp: function(markItUp) {
		window.open('https://functor.network/help/editor?format=tex');
		return false;
	},
	latexBlock: function(markItUp) {
		var textarea=$(markItUp.textarea);
		var lines=markItUp.textarea.value.split(/\n/);
		var caretPosition=markItUp.caretPosition;
		var slectionEnd=caretPosition+markItUp.selection.length;
		var replaceWith,openBlockWith='\n',closeBlockWith='\n';

		if(caretPosition==0) {
			openBlockWith='';
		}
		var length=0;
		$.each(lines,function(index,line){
				length = length + line.length + 1;

					if (length == caretPosition) {
						if (lines[index].trim()) {
							openBlockWith = '';
						} else {
							openBlockWith = '';
						}
					}


				if(length==slectionEnd+1) {
					if (lines.length > index + 1) {
						if (lines[index + 1].trim()) {
							closeBlockWith = '';
						} else {
							closeBlockWith = '';
						}
					}else{
						closeBlockWith = '';
					}
				}
			});
		return {openBlockWith:openBlockWith,closeBlockWith:closeBlockWith};
	},
	latexList:function(markItUp){return this.latexBlock(markItUp);},
	latexHeading:function(markItUp){return this.latexBlock(markItUp);},
	latexLink: function(markItUp) {
		var that=this;
		var prompt = $('<div/>', {
			title:'Hyperlink',
			html:
				'<form><label>URL</label>(<a href="https://functor.network/files" target="_blank">copy a link from file library</a>)<input name="url"  type="text" value="https://example.com" autofocus onfocus="this.select();" class="form-control"/></form>',
		});
		prompt.dialog({
			modal: true,
			buttons: [
				{
					html: "Continue",
					class: "btn btn-primary",
					click: function () {
						var url = $("input[name='url']").val();
						$(this).remove();
						var data={url:url};
						that.latexLinkCallback(markItUp,data);
					},
				},
				{
					html: "Cancel",
					class:"btn btn-default",
					click: function () {
						$(this).remove();
					},
				},
			]
		});
	},
	latexImage: function(markItUp) {
		var that=this;
		var imagePrompt = $('<div/>', {
				title:'Image',
				html:
				'<form><label>Image URL</label>(<a href="https://functor.network/files" target="_blank">copy a link from file library</a>)<input name="image-url"  type="text" value="https://example.com/image.jpg" autofocus onfocus="this.select();" class="form-control"/><label>Image width</label><input name="image-width" class="form-control" placeholder="e.g., 20px, 2em, or 60%"/><label>Image height</label><input name="image-height" class="form-control" placeholder="e.g., 20px, 2em, or 60%"/></form>',
			});
		imagePrompt.dialog({
			modal:true,
			position: {
				my: "center", // Set the dialog to be centered horizontally
				at: "center", // Set the dialog to be centered vertically
				of: window // Set the dialog to be centered within the window
			},
			//open: function () {imagePrompt.tabs();},
			buttons: [
					{
						html: "Continue",
						class:"btn btn-primary",
						click: function () {
								var data={imageURL:$("input[name='image-url']").val(),imageWidth:$("input[name='image-width']").val(),imageHeight:$("input[name='image-height']").val()};
							
								imagePrompt.remove();
								that.latexImageCallback(markItUp, data);	
						},
					},
					{
						html: "Cancel",
						class:"btn btn-default",
						click: function () {
							$(this).remove();
						},
					},
				]
		});
	},
	latexTheorem: function(markItUp) {
		var that=this;
		var prompt = $('<div/>', {
			title:'Theorem-like Environment',
			html:
				'<form><label>environment name</label><input type="text" name="theorem-name" autofocus onfocus="this.select();" value="Theorem" class="form-control"><label>theorem identifier</label><input type="text" name="theorem-identifier" class="form-control"><label>theorem title</label><input type="text" name="theorem-title"  class="form-control"><div><input type="checkbox" name="theorem-number" ><span>auto-numbering</span></div><div class="alert"> A theorem can be linked with <code>[description](#identifier)</code>. Leave the link description blank for auto-reference.</div></form>',
		});
		prompt.dialog({
			modal: true,
			buttons: [
				{
					html: "Continue",
					class: "btn  btn-primary",
					click: function () {
						var data={
							name:$("input[name='theorem-name']").val(),
							title: $("input[name='theorem-title']").val(),
							identifier:$("input[name='theorem-identifier']").val(),
							number: $("input[name='theorem-number']").prop("checked"),
						};
						$(this).remove();
						that.latexTheoremCallback(markItUp,data);
					},
				},
				{
					html: "Cancel",
					class:"btn btn-default",
					click: function () {
						$(this).remove();
					},
				},
			]
		});
	},
	latexPreamble: function(markItUp) {
		var that=this;
		var prompt = $('<div/>', {
			title:'Latex Preamble',
			html:
				'<form>You may include latex packages and define new commands here.<textarea name="temporary" class="form-control"></textarea></form>',
		});
		prompt.dialog({
			modal: true,
			open: function(){$('textarea[name="temporary"]').val($('textarea[name="preamble"]').val());},
			buttons: [
				{
					html: "Continue",
					class: "btn btn-primary",
					click: function () {
						$('textarea[name="preamble"]').val($('textarea[name="temporary"]').val());						
						$(this).remove();
					},
				},
				{
					html: "Cancel",
					class: "btn btn-default",
					click: function () {
						$(this).remove();
					},
				},
			]
		});
	},
	latexCitation: function(markItUp) {
		var that=this;
		var prompt = $('<div/>', {
			title:'Latex Preamble',
			html:
				'<form>Paste citations in the following textarea.<textarea name="temporary" class="form-control" placeholder="@book{identifier,\n	author = {Donald E. Knuth},\n	year = {1986},\n	title = {The {\\TeX} Book},\n	publisher = {Addison-Wesley Professional}\n}\n"></textarea><div class="alert">A reference with identifier <code>identifier</code> can be cited using <code>[@idetifier]</code></div></form>',
		});
		prompt.dialog({
			modal: true,
			open: function(){$('textarea[name="temporary"]').val($('textarea[name="citation"]').val());},
			buttons: [
				{
					html: "Continue",
					class: "btn btn-primary",
					click: function () {
						$('textarea[name="citation"]').val($('textarea[name="temporary"]').val());						
						$(this).remove();
					},
				},
				{
					html: "Cancel",
					class: "btn btn-default",
					click: function () {
						$(this).remove();
					},
				},
			]
		});
	},

	latexNumberedEquation: function(markItUp) {
		var that=this;
		var prompt = $('<div/>', {
			title:'Numbered equation',
			html:
				'<form><label>Equation label</label><input name="equation-label"  type="text" autofocus onfocus="this.select();" class="form-control"/><div class="alert">The equational label is used to identify your equation. You can refer to the eqution with <code>\\eqref{identifier}</code> if you fill in the blank with <code>identifier</code></div></form>',
		});
		prompt.dialog({
			modal: true,
			buttons: [
				{
					html: "Continue",
					class: "btn btn-primary",
					click: function () {
						var data ={label: $("input[name='equation-label']").val()};
						$(this).remove();
						that.latexNumberedEquationCallback(markItUp,data);
					},
				},
				{
					html: "Cancel",
					class:"btn btn-default",
					click: function () {
						$(this).remove();
					},
				},
			]
		});
	},
	
	latexAlignedEquation: function(markItUp) {
		var that=this;
		that.latexAlignedEquationCallback(markItUp);
	},

	latexLinkCallback: function (markItUp,data) {
		$.markItUp({openWith:'\\href{'+data.url+'}{', closeWith:'}', placeHolder:'Your text to link here...',});
		return false;
	},

	latexImageCallback: function (markItUp,data) {
		var openBlockWith='';
		var closeBlockWith='';
		if(markItUp.selection!=""){
			openBlockWith=markIt.latexBlock(markItUp).openBlockWith + '\\begin{figure}[h]\n';
			closeBlockWith='\n\\end{figure}'+ markIt.latexBlock(markItUp).closeBlockWith;
			if (data.imageWidth && data.imageHeight){
				$.markItUp({openBlockWith:openBlockWith,closeBlockWith:closeBlockWith,openWith:'\\includegraphics[width='+data.imageWidth+', height='+data.imageHeight+']{'+data.imageURL+'}\n\\caption{', closeWith:'}', placeHolder:"Your caption here..."});
			}else{
				if(data.imageHeight){
					$.markItUp({openBlockWith:openBlockWith,closeBlockWith:closeBlockWith,openWith:'\\includegraphics[height='+data.imageHeight+']{'+data.imageURL+'}\n\\caption{', closeWith:'}', placeHolder:"Your caption here..."});
				}
				if(data.imageWidth){
					$.markItUp({openBlockWith:openBlockWith,closeBlockWith:closeBlockWith,openWith:'\\includegraphics[width='+data.imageWidth+']{'+data.imageURL+'}\n\\caption{', closeWith:'}', placeHolder:"Your caption here..."});
				}
				if(!data.imageHeight && !data.imageWidth){
					$.markItUp({openBlockWith:openBlockWith,closeBlockWith:closeBlockWith,openWith:'\\includegraphics{'+data.imageURL+'}\n\\caption{', closeWith:'}', placeHolder:"Your caption here..."});
				}
			}

		}else{

			if (data.imageWidth && data.imageHeight){
				$.markItUp({openWith:'\\includegraphics[width='+data.imageWidth+', height='+data.imageHeight+']{'+data.imageURL+'}', closeWith:'',});
			}else{
				if(data.imageHeight){
					$.markItUp({openWith:'\\includegraphics[height='+data.imageHeight+']{'+data.imageURL+'}', closeWith:'',});
				}
				if(data.imageWidth){
					$.markItUp({openWith:'\\includegraphics[width='+data.imageWidth+']{'+data.imageURL+'}', closeWith:'',});
				}
				if(!data.imageHeight && !data.imageWidth){
					$.markItUp({openWith:'\\includegraphics{'+data.imageURL+'}', closeWith:'',});
				}
			}
		}
		return false;
	},

	latexNumberedEquationCallback: function (markItUp,data) {
		var openBlock=markIt.latexBlock(markItUp).openBlockWith + '\\begin{equation}';
		var closeBlock='\n\\end{equation}'+ markIt.latexBlock(markItUp).closeBlockWith;
		if(data.label){
			$.markItUp({		
				openBlockWith:openBlock,closeBlockWith:closeBlock,
				openWith:'\\label{'+ data.label +'}\n',placeHolder:'latex code for your equation',
			});
		}else{
			$.markItUp({openBlockWith:openBlock,closeBlockWith:closeBlock,openWith:'\n',placeHolder:'latex code for your equation',});
		}		
		return false;
	},

	latexTheoremCallback: function(markItUp,data){
		var attr=".theorem-like";
		if(data.identifier){attr=attr+" #"+data.identifier}
		if(data.name){attr=attr+' name="'+data.name+'"'}
		if(data.title){attr=attr+' title="'+data.title+'"'}
		if(data.number){attr=attr+" .auto-numbering"}
		$.markItUp({openWith:':::{'+attr+'}\n', closeWith:'\n:::', placeHolder:'Your theorem content goes here...',});
		return false;
	}

};
