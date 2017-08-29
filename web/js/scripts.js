var files = [];

try {
    var host = "ws://localhost:31415";
    console.log("Host:", host);
    
    var s = new WebSocket(host);

    s.onopen = function (e) {
        console.log("Socket opened.");
    };

    s.onclose = function (e) {
        console.log("Socket closed.");
    };

    s.onmessage = function (e) {    // When received a message from the server
        if (e.data.substr(0,5) == "input"){
            editor.setValue(e.data.substr(5,e.data.length));
            editor.selection.setRange({start : {row:0, column:0}, end : {row:0, column:0}});
        } else if (e.data.substr(0,5) == "files"){
            files = [];
            filesRaw = e.data.substr(5,e.data.length).split("@");
            for (var i = 0; i < filesRaw.length; i++){
                files[i] = { 'name': filesRaw[i], 'active' : "" };
            }
        } else if (e.data.substr(0,5) == "error"){
            document.getElementById("resultsbox").innerHTML = e.data.substr(5,e.data.length);
        } else if (e.data.substr(19,4) == "text") {
            katex.render(e.data, document.getElementById("vars-and-params"), { displayMode: false });
        } else {
            katex.render(e.data, document.getElementById("resultsbox"), { displayMode: false });
        }
    };

    s.onerror = function (e) {
        console.log("Socket error:", e);
    };

} catch (ex) {
    console.log("Socket exception:", ex);
};

function showFiles(){
    document.getElementById("modal-title").innerHTML = "Select a file";
    document.getElementById("modalBody").innerHTML = ""
    for (var i = 0; i < files.length; i++){
        document.getElementById("modalBody").innerHTML +=
            '<div id="' + files[i].name + '" class="container-fluid ' + files[i].active +
            '"><a href="#" onClick="openFile(\'' + files[i].name + '\')">' + files[i].name + '</a></div>';
    }
}

function openFile(f){
    $("#modalBody").find("div").removeClass("active");
    document.getElementById(f).classList.add("active");
    for (var i = 0; i < files.length; i++){
        if (files[i].name == f){ files[i].active = "active"; }
        else { files[i].active = ""; }
    }
    s.send('open' + f);
}

function showHelp(){
    document.getElementById("modal-title").innerHTML = "Help"
    document.getElementById("modalBody").innerHTML =
        '<ul>' +
        '<li><kbd>Ctrl + <span class="glyphicon glyphicon-arrow-up" aria-hidden="true"></kbd> Eval previous command</li>' +
        '<li><kbd>Ctrl + Enter</kbd> Eval current command</li>' + 
        '<li><kbd>Ctrl + <span class="glyphicon glyphicon-arrow-down" aria-hidden="true"></kbd> Eval ne+xt command</li>' +
        '<li><kbd>Ctrl + Alt + o</kbd> Open file</li>' +
        '<li><kbd>Ctrl + Alt + h</kbd> Show help</li>' + 
        '</ul>'
}

function init(){
    try { s.send("getinput"); s.send('files'); }
    catch (ex) { setTimeout(function(){ init(); }, 50); };
}

init();


//key bindings
var map = {};
onkeydown = onkeyup = function(e){
    e = e || event; // to deal with IE
    map[e.keyCode] = e.type == 'keydown';
    
    if (map[17] && map[18] && map[79]){ // Cntr + Alt + o
        document.getElementById("openBtn").click();
    } else if (map[17] && map[18] && map[72]){ // Cntr + Alt +  h
        document.getElementById("helpBtn").click();
    } else if (map[17] || map[18]){
        
    } else {
        setTimeout(function(){ map = {};}, 200);
    }
}

