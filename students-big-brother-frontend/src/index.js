"use strict"

// const $ = require('jquery');
import $ from 'jquery'
import  _ from 'lodash'
import './style.css'
// import cfg from 'cfg.json'

const cfg = JSON.parse(require("./cfg.json"))
const hostname = cfg.hostname

// const hostname = "http://ec2-54-186-163-30.us-west-2.compute.amazonaws.com/"

// module.exports = function () {
$("document-ready", function () {
    $("#login-button").on('click', async function(e) {
        const username = $("#login-username").val();
        const password = $("#login-password").val();

        let t = await retrieveStudentsData(username, password)

        updateUI(t)

        // retrieveStudentsData(username, password)();
        $("#students-list-refresh-button").on('click', async function(e) {
          t = await retrieveStudentsData(username, password)
          updateUI(t)
        });
        // // setInterval(retrieveStudentsData(username, password), 5000);
    });
})

// Authenticate teacher and ask server for array of students data
async function retrieveStudentsData(username, password) {
  const fetchOptions = { method: "GET"
                       , headers: { 'Accept': 'application/json'
                                  , "Authorization": "Basic " +
                                     btoa(username + ":" + password)
                         }
                       , mode: "cors"
                       }
  const errorHandler = error => {
    console.log(error)
    switch(error.code) {
        case 500:
            $("#500-warning").show()
            break;
        case 401, 403:
            $("#401-403-warning").show()
            break;
        default:
            $("#unknown-error-warning").show()
    }
  }
  return await fetch("http://" + hostname + ":8083/files", fetchOptions)
                    .then(response => response.json())
                    .catch(errorHandler)
}

function updateUI(result) {
      // hide warnings if presented
      $(".warning").hide();

      // hide login form
      $("#login-form").hide();

      // show refresh button
      $("#students-list-refresh-button").show();

      var sourceFiles = result
      // hide warnings if presented
      $(".warning").hide();

      // hide login form
      $("#login-form").hide;
      // calculate unique user ids

      var uids = calculateUniqueIDs(sourceFiles)

      if (uids.length == 0) {
          $("#no-students-warning").show();
      } else {
          var groupedFiles = _.chain(sourceFiles)
              .groupBy(function(x) {return x.student.s_id;})
              .value();

          // delete all tabs from preveous state
          // TODO: refactor this shit, it's terribale; It would be good ta have state monad here...
          $("#tabs").empty();
          $("#tabs-contents").empty();

          // build students list on the ui
          var studentTabs = document.getElementById("tabs");
          var studentTabsContents =
              document.getElementById("tabs-contents");

          renderStudentsTabsList(uids, groupedFiles, studentTabs);

          renderStudentsTabsContents(uids, groupedFiles, studentTabsContents);

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

function renderStudentsTabsContents(uids, groupedFiles, studentTabsContents) {
  _.forEach(uids, function(uid) {
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
}

function renderStudentsTabsList(uids, groupedFiles, studentTabs) {
  _.forEach(uids, function(uid) {
    // students tabs list
    var studentTab = document.createElement("li");
    var a = document.createElement('a');
    a.href =  "#tab-" + uid;
    a.innerHTML = groupedFiles[uid][0].student.l_name;
    studentTab.appendChild(a);
    studentTabs.appendChild(studentTab);
  });
}

function calculateUniqueIDs(sourceFiles) {
  return _.chain(sourceFiles)
          .map(function(file) {
             return file.student.s_id;
          })
          .uniq()
          .sortBy()
          .value();
}

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
