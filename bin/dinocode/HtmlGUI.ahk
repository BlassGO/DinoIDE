; HtmlGUI addon by @BlassGO

htmlGui(title:="", html:="", css:="", js:="", w:="500", h:="300") {
   return gui:=new NeutronWindow(html, css, js, title), gui.Show("w" . w . " " . "h" . h)
}
dinocode(options*) {
   (tag:=(IsObject(options.2))?options.3:options.2)?load_config("section " . tag, true, {html:options.1, event:options.2}):false
}