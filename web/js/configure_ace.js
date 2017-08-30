
function select_up_to_cursor(){
    var cursor = editor.selection.getCursor();
    var currentline = editor.session.getLine(cursor.row);
    editor.selection.setRange({start : {row:0, column:0}, end : {row:cursor.row, column:currentline.length}});
};

function select_up_to_next_line(){
    var next_row = editor.selection.getCursor().row+1;
    var line = editor.session.getLine(next_row);
    editor.selection.setRange({start : {row:0, column:0}, end : {row:next_row, column:line.length}});
};

function select_up_to_previous_line(){
    var next_row = editor.selection.getCursor().row-1;
    var line = editor.session.getLine(next_row);
    editor.selection.setRange({start : {row:0, column:0}, end : {row:next_row, column:line.length}});
};

function select_up_to_next_dot(){
    var cursor = editor.selection.getCursor();
    var col = cursor.column-1;
    for (var row = 0; row < 1000; row++){
        var line = editor.session.getLine(row + cursor.row);
        var nextDot = -1;
        for (var i = col; i < line.length; i++){
            if (line[i] == "."){ nextDot = i; break }
        }
        if (nextDot != -1){ break; }
        col = 0;
    }
    editor.selection.setRange({start : {row:0, column:0}, end : {row:(row+cursor.row), column:(nextDot+1)}});
};

function select_forwards(){
    var cursor = editor.selection.getCursor();
    var col = cursor.column;
    for (var row = 0; row < 1000; row++){
        var line = editor.session.getLine(row + cursor.row);
        var nextDot = -1;
        for (var i = col; i < line.length; i++){
            if (line[i] == "."){ nextDot = i; break }
        }
        if (nextDot != -1){ break; }
        col = 0;
    }
    editor.selection.setRange({start : {row:0, column:0}, end : {row:(row+cursor.row), column:(nextDot+1)}});
};

function select_backwards(){
    var cursor = editor.selection.getCursor();
    var a = 1;
    for (var row = 0; row < 1000; row++){
        var line = editor.session.getLine(cursor.row-row);
        var nextDot = -1;
        var col;
        if (a == 0){ col = line.length; } else { col = cursor.column-2 }
        for (var i = col; i >= 0; i--){
            if (line[i] == "."){ nextDot = i; break }
        }
        if (nextDot != -1){ break; }
        a = 0;
    }
    editor.selection.setRange({start : {row:0, column:0}, end : {row:(cursor.row-row), column:(nextDot+1)}});
};

function analyze_selection(){
    var input = editor.getSession().doc.getTextRange(editor.selection.getRange());
    s.send(input);
};

var divs_height = Math.round(0.95 * $(document).height());
document.getElementById("editor-div").style = 'height: ' + divs_height + 'px;';
document.getElementById("outputs-div").style = 'height: ' + divs_height + 'px;';
document.getElementById("resultsbox").style =
    'overflow:auto; color:white; margin:30px; width:95%; height:' + 0.75*divs_height + 'px;';

var editor = ace.edit("editor");
editor.setTheme("ace/theme/ggmss");
editor.getSession().setMode("ace/mode/ocaml");
editor.$blockScrolling = Infinity;


editor.commands.addCommand({
    name: "Analyze",
    bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
    exec: function(editor){ select_up_to_next_dot(); analyze_selection(); },
    readOnly: true
});

editor.commands.addCommand({
    name: "Next line",
    bindKey: {win: 'Ctrl-Down',  mac: 'Command-Down'},
    exec: function(editor){ select_forwards(); analyze_selection(); },
    readOnly: true
});

editor.commands.addCommand({
    name: "Pevious line",
    bindKey: {win: 'Ctrl-Up',  mac: 'Command-Up'},
    exec: function(editor){ select_backwards(); analyze_selection(); },
    readOnly: true
});
