function copy_to_clipboard(element) {
    // Assume the link is embedded in the codeblock
    while (! element.classList.contains("codeblock")) {
        element = element.parentElement;
    }

    var temp = $("<textarea>");
    $("body").append(temp);
    // Remove line numbers
    var code = $("pre", element)
        .clone()
        .find(".lineno")
        .remove()
        .end()
        .text();
    temp.val(code).select();
    document.execCommand("copy");
    temp.remove();
}

$(document).ready(function(){

    // Make navigation bar sticky
    // based on http://www.bennadel.com/blog/1810-creating-a-sometimes-fixed-\
    //                                       position-element-with-jquery.htm
    var navbar = $("#navbar");
    var placeholder = $("#navbar-placeholder");
    // Cache jQuery-enriched window object
    var view = $(window);

    view.bind("scroll resize", function () {
        var scroll = view.scrollTop();
        var offset = placeholder.offset().top;
        if (scroll > offset && ! navbar.hasClass("fixed")) {
            placeholder.height(placeholder.height());
            navbar.addClass("fixed");
        } else if (scroll <= offset && navbar.hasClass("fixed")) {
            placeholder.css("height", "auto");
            navbar.removeClass("fixed");
        }
    });


    // Make search button work
    var link = $('#search-link');
    var form = $('form.search');
    // Toggle search field visibility
    link.click(function () {
        if (form.is(':visible')) {
            form.fadeOut('fast');
        } else {
            form.fadeIn('fast');
            form.find('input').focus();
        }
    });
    // Use Google for searching
    form.submit(function () {
        i = form.find('#input');
        q = form.find('#query');
        q.val('site:br0ns.dk ' + i.val());
    });

    // Add copy'n'paste link to code blocks
    $("figure.codeblock:not(.no-copynpaste) .buttons-container").prepend(
        '<a onclick="copy_to_clipboard(this);" title="Copy to clipboard"><i class="fa fa-clipboard"></a>'
    );

    // Zoom scaled images when clicking
    hs.dimmingOpacity = 0.75;
    hs.dimmingDuration = 0;
    hs.expandCursor = null;
    $("img").load(function () {
        var img = this;
        $("<img/>")
            .attr("src", img.src)
            .load(function () {
                if (this.width > img.width) {
                    img.onclick = function () {
                        hs.expand(img, {src: img.src});
                    };
                }
            });
    });

});
