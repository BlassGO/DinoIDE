; HtmlGUI addon

htmlGui(title:="", html:="", css:="", js:="", w:="500", h:="300") {
   return gui:=new NeutronWindow(html, css, js, title), gui.Show("w" . w . " " . "h" . h)
}
dinocode(options*) {
   (tag:=(IsObject(options[2])) ? options[3] : options[2]) ? load_config("section " . tag, true, {html: options[1] ,event: options[2]}) : false
}
web_code(url, time:=false) {
   return load_config(responseText(url,time),,, FD_CURRENT)
}
responseText(url, time:=3000) {
   ComObjError(false)
   get := ComObjCreate("WinHttp.WinHttpRequest.5.1")
   try {
      time ? get.SetTimeouts(time, time, time, time)
      get.Open("GET", url, false)
      get.Send()
      if (get.status = 200)
         return get.responseText, get.Quit
   }
}