$(document).ready(function() {
    $("#login-button").on('click', function(e) {
        var username = $("#login-username").val();
        var password = $("#login-password").val();

        // Authenticate teacher and ask server for array of students data
        var retrieveStudentsData = function(){$.ajax({
            type: "GET",
            url: "http://127.0.0.1:8083/files",
            dataType: "json",
            headers: {
                "Authorization": "Basic " + btoa(username + ":" + password)
              },
            error:
                function(jqXHR, textStatus, errorThrown) {
                    switch(jqXHR.status) {
                        case 500:
                            $("#500-warning").show();
                            break;
                        case 401, 403:
                            $("#401-403-warning").show();
                            break;
                        default:
                            console.log(textStatus + "," +
                                        errorThrown + "," +
                                        jqXHR.responseText);
                            console.log(arguments);
                            $("#unknown-error-warning").show();
                    }
                },
            success:
                function(result) {
                    console.log("privet");
                    // hide warnings if presented
                    $(".warning").hide();

                    // hide login form
                    $("#login-form").hide();

                    var sourceFiles = result
                    console.log("privet");
                    // hide warnings if presented
                    $(".warning").hide();

                    // hide login form
                    $("#login-form").hide;
                    // calculate unique user ids
                    var uids = _.chain(sourceFiles)
                        .map(function(file) {
                            return file.uid;
                        })
                        .uniq()
                        .sortBy()
                        .value();

                    if (uids.length == 0) {
                        $("#no-students-warning").show();
                    } else {
                        var groupedFiles = _.chain(sourceFiles)
                            .groupBy(function(x) {return x.uid;})
                            .value();

                        // delete all tabs from preveous state
                        // TODO: refactor this shit, it's terribale; It would be good ta have state monad here...
                        $("#tabs").empty();
                        $("#tabs-contents").empty();

                        // build students list on the ui
                        var studentTabs = document.getElementById("tabs");
                        var studentTabsContents =
                            document.getElementById("tabs-contents");

                        _.forEach(uids, function(uid) {
                            // students tabs list
                            var studentTab = document.createElement("li");
                            var a = document.createElement('a');
                            a.href =  "#tab-" + uid;
                            a.innerHTML = "Student " + uid;
                            studentTab.appendChild(a);
                            studentTabs.appendChild(studentTab);

                            // students tabs contents
                            var tabContent = document.createElement("div");
                            tabContent.id = "tab-" + uid;
                            tabContent.className = "tab-content";
                            var filesOfThisUser = _.chain(groupedFiles[uid])
                                .map(function (x) {
                                    return x.file;})
                                .map(sourceFileJSONtoDOM)
                                .map(function(fileNode) {
                                    var li = document.createElement("li");
                                    li.appendChild(fileNode);
                                    return li;
                                })
                                .value();
                            var ul = document.createElement("ul");
                            _.forEach(filesOfThisUser, function(li) {
                                ul.appendChild(li);
                            });
                            tabContent.appendChild(
                                ul
                            );
                            studentTabsContents.appendChild(tabContent);
                        });

                        $(".tabs-menu a").click(function(event) {
                            $(".tab").css("display", "block");
                            event.preventDefault();
                            $(this).parent().addClass("current");
                            $(this).parent().siblings().removeClass("current");
                            var tab = $(this).attr("href");
                            $(".tab-content").not(tab).css("display", "none");
                            $(tab).fadeIn();
                        });
                    }
                }
        }
      );}
        retrieveStudentsData();
        setInterval(retrieveStudentsData, 5000);
    });
});

function sourceFileJSONtoDOM(file) {
    var h = document.createElement("h3");
    h.innerHTML = file.path;
    var code = document.createElement("code");
    code.appendChild(document.createTextNode(file.contents));
    var pre = document.createElement("pre");
    pre.appendChild(code);
    var result = document.createElement("div");
    result.appendChild(h);
    result.appendChild(pre);

    hljs.configure({
      languages: ["haskell", "cpp", "c", "h", "hpp",
                  "pascal", "java", "csharp", "python",
                  "html", "css", "js"]
    });
    hljs.highlightBlock(pre);

    return result;
}
