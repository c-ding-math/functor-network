// ----------------------------------------------------------------------------
// markItUp!
// ----------------------------------------------------------------------------
// Copyright (C) 2008 Jay Salvat
// http://markitup.jaysalvat.com/
// ----------------------------------------------------------------------------
myMarkdownSettings = {
    nameSpace:          'markdown', // Useful to prevent multi-instances CSS conflict
	onCtrlEnter:        {keepDefault:false, call:'preview'}, //not working
    markupSet: [
    
        {name:'Math', className:'', dropMenu:[
			{name:'Inline math', key:"1", openWith:'$', closeWith:'$',placeHolder:'latex codes'},
            {name:'Display math', key:"2", openWith:'$$', closeWith:'$$',placeHolder:'latex codes'},
			{name:'Numbered equation', key:"3", replaceWith:function(markItUp){markIt.markdownNumberedEquation(markItUp);return false;},},
			{name:'Aligned equations', key:"4", openWith:'\\begin{align*}\n', closeWith:'\n\\end{align*}',placeHolder:'& first equation\\\\\n & second equation'},
			{name:'Equation reference', key:"5", openWith:'\\eqref{', closeWith:'}',placeHolder:'equation label'},
        ]},
		{name:'Meta', dropMenu:[
			{
				name:'Latex preamble', key:"8", replaceWith:function(markItUp){markIt.markdownPreamble(markItUp);return false;},
			},
			{name:'Bibliography', key: "9", className:'',replaceWith:function(markItUp){markIt.markdownCitation(markItUp);return false;},},
		]},
        
        {name:'Help', className:'', dropMenu:[
			{name:'Syntax help',replaceWith:function(markItUp){markIt.markdownSyntaxHelp(markItUp);return false;},},	
			{name:'Editor help',replaceWith:function(markItUp){markIt.markdownEditorHelp(markItUp);return false;},},
        ]},

        {name:'Preview', className:"preview", dropMenu:[
			{name:"Update preview", key:'0', call:'preview'},
			//{name:"Shrink", key:"", replaceWith:function(markItUp){markIt.editorShrink(markItUp);return false;},className:"normal-size hidden"},
			{name:"Side by side", key:'M', className:'toggle', call:'preview',replaceWith:function(markItUp){markIt.editorToggle(markItUp);return false;}},
		]},
      
    ]
}
markIt = {
	editorToggle: function(markItUp) {
		$(".markItUpContainer").toggleClass("full-screen");
		if($(".markItUpContainer").hasClass("full-screen")) {
			$(".markItUpContainer .toggle a").text("Normal");
		} else {
			$(".markItUpContainer .toggle a").text("Side by side");
		}
		return false;
	},
	markdownSyntaxHelp: function(markItUp) {
		window.open('https://www.functor.network/LaTeX%20Help');
		return false;
	},
	markdownEditorHelp: function(markItUp) {
		window.open('https://functors.net/help/Edit%20Help');
		return false;
	},
	markdownQuote: function(markItUp) {
		var textarea=$(markItUp.textarea);
		var lines=markItUp.textarea.value.split(/\n/);
		var caretPosition=markItUp.caretPosition;
		var slectionEnd=caretPosition+markItUp.selection.length;
		var replaceWith,openBlockWith='\n\n',closeBlockWith='\n\n';

		if(caretPosition==0) {
			openBlockWith='';
		}
		var length=0;
		$.each(lines,function(index,line){
				length = length + line.length + 1;

					if (length == caretPosition) {
						if (lines[index].trim()) {
							openBlockWith = '\n';
						} else {
							openBlockWith = '';
						}
					}


				if(length==slectionEnd+1) {
					if (lines.length > index + 1) {
						if (lines[index + 1].trim()) {
							closeBlockWith = '\n';
						} else {
							closeBlockWith = '';
						}
					}else{
						closeBlockWith='';
					}
				}
			});
		return {openBlockWith:openBlockWith,closeBlockWith:closeBlockWith};
	},
	markdownList:function(markItUp){return this.markdownQuote(markItUp);},
	markdownHeading:function(markItUp){return this.markdownQuote(markItUp);},
	markdownLink: function(markItUp) {
		var that=this;
		var prompt = $('<div/>', {
			title:'Hyperlink',
			html:
				'<form><label>URL</label>(<a href="https://functors.net/files" target="_blank">copy a link from file library</a>)<input name="url"  type="text" value="https://example.com" autofocus onfocus="this.select();" class="form-control"/></form>',
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
						that.markdownLinkCallback(markItUp,data);
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
	markdownImage: function(markItUp) {
		var that=this;
		var imagePrompt = $('<div/>', {
				title:'Image',
				html:
				'<form><label>Image URL</label>(<a href="https://functors.net/files" target="_blank">copy a link from file library</a>)<input name="image-url"  type="text" value="https://example.com/image.jpg" autofocus onfocus="this.select();" class="form-control"/><label>Image width</label><input name="image-width" class="form-control" placeholder="e.g., 20px, 2em, or 60%"/></form>',
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
								var data={imageURL:$("input[name='image-url']").val(),imageWidth:$("input[name='image-width']").val()};
							
								imagePrompt.remove();
								that.markdownImageCallback(markItUp, data);	
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
	markdownTheorem: function(markItUp) {
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
						that.markdownTheoremCallback(markItUp,data);
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
	markdownPreamble: function(markItUp) {
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
	markdownCitation: function(markItUp) {
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

	markdownNumberedEquation: function(markItUp) {
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
						that.markdownNumberedEquationCallback(markItUp,data);
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
	
	markdownLinkCallback: function (markItUp,data) {
		$.markItUp({openWith:'[', closeWith:']('+data.url+')', placeHolder:'Your text to link here...',});
		return false;
	},

	markdownImageCallback: function (markItUp,data) {
		if(data.imageWidth){
			$.markItUp({openWith:'![', closeWith:']('+data.imageURL+'){width='+data.imageWidth+'}', placeHolder:'Your text to link here...',});
		}else{
			$.markItUp({openWith:'![', closeWith:']('+data.imageURL+')', placeHolder:'Your text to link here...',});
		}		
		return false;
	},

	markdownNumberedEquationCallback: function (markItUp,data) {
		if(data.label){
			$.markItUp({openWith:'\\begin{equation}\\label{'+ data.label +'}\n', closeWith:'\n\\end{equation}', placeHolder:'latex code for your equation',});
		}else{
			$.markItUp({openWith:'\\begin{equation}\n', closeWith:'\n\\end{equation}', placeHolder:'latex code for your equation',});
		}		
		return false;
	},

	markdownTheoremCallback: function(markItUp,data){
		var attr=".theorem-like";
		if(data.identifier){attr=attr+" #"+data.identifier}
		if(data.name){attr=attr+' name="'+data.name+'"'}
		if(data.title){attr=attr+' title="'+data.title+'"'}
		if(data.number){attr=attr+" .auto-numbering"}
		$.markItUp({openWith:':::{'+attr+'}\n', closeWith:'\n:::', placeHolder:'Your theorem content goes here...',});
		return false;
	}

};