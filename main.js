/* This file automatically creates a simple blog engine for navigating my blog posts with tags */
/* Copyright (c) 2012 Pavel Panchekha. This code is under the MIT license.*/

// Firefox and Chrome are buggy in some way that makes this necessary
function $(id) {return document.getElementById(id);}
function $$(query, base) {return (base || document).querySelectorAll(query);}

// handle code blocks
window.addEventListener("load", function() {
    var code_blocks = document.getElementsByTagName('pre');
    for (var i = 0; i < code_blocks.length; i++) {
        var class_name = code_blocks[i].className;
        var new_syntax = "nohighlight";
        if (class_name && 0 !== class_name.length) {
            var srcs = class_name.trim().split(/\s+/);
            for (var j=0; j<srcs.length; j++) {
                if (srcs[j].startsWith("src-")) {
                    new_syntax = srcs[j].substring(4);
                    break;
                }
            }
        }

        // console.log(code_blocks[i].innerHTML);
        // console.log(new_syntax);
        
        code_blocks[i].outerHTML =
            "<pre><code class='" +
            new_syntax +
            "'>" +
            code_blocks[i].innerHTML +
            "</code></pre>";

        hljs.highlightBlock(code_blocks[i]);
    }
}, false);

window.addEventListener("load", function() {
    var header    = document.getElementById('org-div-home-and-up');
    var up_button = header.children[0];
    var up_href   = window.location.pathname.match(/.*\//);
    up_button.href = window.location.origin + up_href;
}, false);
