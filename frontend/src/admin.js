"use strict"

import $ from 'jquery'
import _ from 'lodash'
import './style.css'

var cfg = JSON.parse(require("./cfg.json"))
const hostname = cfg.hostname

$(document).ready(main);

async function main() {
  const fetchOptions = { method: "GET"
                      //  , headers: { 'Accept': 'application/json'
                      //             , "Authorization": "Basic " +
                      //                btoa(username + ":" + password)
                      //  }
                       , mode: "cors"
                       }
  const errorHandler = error => {
    console.log(error)
  }
  const teachers = await fetch( "http://" + "localhost"
                                          + ":8083/admin/list-teachers"
                              , fetchOptions)
                              .then(response => response.json())
                              .catch(errorHandler)
  drawTable(teachers);
  $("#add-teacher-button").click(function() {
      var uname = $("#new-teacher-username").val();
      var pwd = $("#new-teacher-password").val();
      if (isInvalid(uname) || isInvalid(pwd)) {
        alert("Username or Password is invalid!");
        return;
      }
      var newTeacher = { username:uname
                       , password:pwd
                       };
      $.ajax({
        type: "POST",
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        success: function(justAddedTeacher) {
          drawRow(justAddedTeacher)
        },
        url: "http://" + hostname + ":8083/admin/register-teacher",
        data: JSON.stringify(newTeacher),
      })
  })
  $('#teachers-list-table').on('click', '.delete-button', function() {
     const rowToDelete = $(this).parent().parent()
     const teacherToDeleteID = rowToDelete.find("td").first().html()
     $.ajax({
       type: "POST",
       headers: {
           'Accept': 'application/json',
           'Content-Type': 'application/json'
       },
       success: function() {
         rowToDelete.remove();
       },
       url: "http://" + hostname + ":8083/admin/delete-teacher",
       data: teacherToDeleteID,
     })
  })
}

function drawTable(data) {
    for (var i = 0; i < data.length; ++i) {
        drawRow(data[i]);
    }
}

function drawRow(rowData) {
    var row = $("<tr />")
    // console.log($("#teachers-list-table:last"));
    // $("#teachers-list-table").append(row); //this will append tr element to table... keep its reference for a while since we will add cels into it
    row.append($("<td>" + rowData.teacher_id + "</td>"));
    row.append($("<td>" + rowData.teacher_credential.username + "</td>"));
    row.append($("<td>" + rowData.teacher_credential.password + "</td>"));
    row.append(
      $("<td><button class=\"delete-button\">Delete</button></td>")
    );
    $("#teachers-list-table").append(row);
}

function isInvalid(input) {
  input = input.trim();
  return (!input || input.length === 0);
}
