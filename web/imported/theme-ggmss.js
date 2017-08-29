ace.define("ace/theme/ggmss",["require","exports","module","ace/lib/dom"], function(require, exports, module) {

exports.isDark = true;
exports.cssClass = "ace-ggmss";
exports.cssText = ".ace-ggmss .ace_gutter {\
background: #272727;\
color: #FFF\
}\
.ace-ggmss .ace_print-margin {\
width: 1px;\
background: #272727\
}\
.ace-ggmss {\
background-color: #2D2D2D;\
color: #DDD\
}\
.ace-ggmss .ace_constant.ace_other,\
.ace-ggmss .ace_cursor {\
color: #CCCCCC\
}\
.ace-ggmss .ace_marker-layer .ace_selection {\
background: #515151\
}\
.ace-ggmss.ace_multiselect .ace_selection.ace_start {\
box-shadow: 0 0 3px 0px #2D2D2D;\
}\
.ace-ggmss .ace_marker-layer .ace_step {\
background: rgb(102, 82, 0)\
}\
.ace-ggmss .ace_marker-layer .ace_bracket {\
margin: -1px 0 0 -1px;\
border: 1px solid #6A6A6A\
}\
.ace-tomorrow-night-bright .ace_stack {\
background: rgb(66, 90, 44)\
}\
.ace-ggmss .ace_marker-layer .ace_active-line {\
background: #393939\
}\
.ace-ggmss .ace_gutter-active-line {\
background-color: #393939\
}\
.ace-ggmss .ace_marker-layer .ace_selected-word {\
border: 1px solid #515151\
}\
.ace-ggmss .ace_invisible {\
color: #6A6A6A\
}\
.ace-ggmss .ace_keyword,\
.ace-ggmss .ace_meta,\
.ace-ggmss .ace_storage,\
.ace-ggmss .ace_storage.ace_type,\
.ace-ggmss .ace_support.ace_type {\
color: #FFC1FF\
}\
.ace-ggmss .ace_keyword.ace_operator {\
color: #66CCCC\
}\
.ace-ggmss .ace_zp {\
color: #C29BF5;\
font-weight: bold;\
}\
.ace-ggmss .ace_quantifications {\
color: #F78282\
}\
.ace-ggmss .ace_rules {\
color: #F5F6CE\
}\
.ace-ggmss .ace_bigops {\
color: #F5A9BC\
}\
.ace-ggmss .ace_constant.ace_character,\
.ace-ggmss .ace_constant.ace_language,\
.ace-ggmss .ace_constant.ace_numeric,\
.ace-ggmss .ace_keyword.ace_other.ace_unit,\
.ace-ggmss .ace_support.ace_constant,\
.ace-ggmss .ace_variable.ace_parameter {\
color: #F7BE81\
}\
.ace-ggmss .ace_invalid {\
color: #CDCDCD;\
background-color: #F2777A\
}\
.ace-ggmss .ace_invalid.ace_deprecated {\
color: #CDCDCD;\
background-color: #CC99CC\
}\
.ace-ggmss .ace_fold {\
background-color: #6699CC;\
border-color: #CCCCCC\
}\
.ace-ggmss .ace_entity.ace_name.ace_function,\
.ace-ggmss .ace_support.ace_function,\
.ace-ggmss .ace_variable {\
color: #81DAF5\
}\
.ace-ggmss .ace_support.ace_class,\
.ace-ggmss .ace_support.ace_type {\
color: #FFCC66\
}\
.ace-ggmss .ace_heading,\
.ace-ggmss .ace_markup.ace_heading,\
.ace-ggmss .ace_string {\
color: #99CC99\
}\
.ace-ggmss .ace_comment {\
color: #999999\
}\
.ace-ggmss .ace_entity.ace_name.ace_tag,\
.ace-ggmss .ace_entity.ace_other.ace_attribute-name,\
.ace-ggmss .ace_meta.ace_tag,\
.ace-ggmss .ace_variable {\
color: #F2777A\
}\
.ace-ggmss .ace_indent-guide {\
background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAACCAYAAACZgbYnAAAAEklEQVQImWPQ09NrYAgMjP4PAAtGAwchHMyAAAAAAElFTkSuQmCC) right repeat-y\
}";

var dom = require("../lib/dom");
dom.importCssString(exports.cssText, exports.cssClass);
});
