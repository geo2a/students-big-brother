"use strict"

import $ from 'jquery'
import _ from 'lodash'
import './style.css'

const cfg = JSON.parse(require("./cfg.json"))
const admin_uname = "geo2a"
const admin_pwd = "123"
const hostname = cfg.hostname
const errorHandler = error => {
  console.log(error)
}

$("document-ready", function () {
    $("#login-button").on('click', main);
})

async function main() {
  $("#login-form").hide()
  $(".teachers-list-container").show()
  const admin_uname = $("#login-username").val()
  const admin_pwd = $("#login-password").val()
  const fetchOptions = { method: "GET"
                       , mode: "cors"
                       , headers: { "Accept": "application/json"
                                  , "Authorization": "Basic " +
                                     btoa(admin_uname + ":" + admin_pwd)
                                  }
                       }
  const teachers = await fetch( "http://" + "localhost"
                                          + ":8083/admin/list-teachers"
                              , fetchOptions)
                              .then(response => response.json())
                              .catch(errorHandler)
  drawTable(teachers)
  $(".add-button").off("click").on("click", addTeacher)
  $(".delete-button").off("click").on("click", deleteTeacher)
}

async function addTeacher() {
    const uname = $("#new-teacher-username").val()
    const pwd = $("#new-teacher-password").val()
    if (isInvalid(uname) || isInvalid(pwd)) {
      alert("Username or Password is invalid!")
      return;
    }
    const newTeacher = { username:uname
                       , password:pwd
                       }

    const fetchOptions = { method: "POST"
                         , headers: { "Accept": "application/json"
                                    , "Content-Type": "application/json"
                                    , "Authorization": "Basic " +
                                       btoa(admin_uname + ":" + admin_pwd)
                                    }
                         , body: JSON.stringify(newTeacher)
                         , mode: "cors"
                         }
    const justAddedTeacher =
      await fetch( "http://" + hostname + ":8083/admin/register-teacher"
                 , fetchOptions)
                 .then(responce => responce.json())
                 .catch(errorHandler)
    drawRow(justAddedTeacher)
    $(".delete-button").off("click").on("click", deleteTeacher)
}

async function deleteTeacher() {
   const rowToDelete = $(this).parent().parent()
   const teacherToDeleteID = rowToDelete.find("td").first().html()
   $.ajax({
     type: "POST",
     headers: {
         'Accept': 'application/json',
         'Content-Type': 'application/json'
         , "Authorization": "Basic " +
            btoa(admin_uname + ":" + admin_pwd)
     },
     success: function() {
       rowToDelete.remove();
     },
     url: "http://" + hostname + ":8083/admin/delete-teacher",
     data: teacherToDeleteID,
   })
}

function drawTable(data) {
    for (let i = 0; i < data.length; ++i) {
        drawRow(data[i])
    }
}

function drawRow(rowData) {
    let row = $("<tr />")
    row.append($("<td>" + rowData.teacher_id + "</td>"))
    row.append($("<td>" + rowData.teacher_credential.username + "</td>"))
    row.append($("<td>" + rowData.teacher_credential.password + "</td>"))
    row.append(
      $("<td><button class=\"delete-button\">Delete</button></td>")
    )
    $("#teachers-list-table").append(row)

}

function isInvalid(input) {
  input = input.trim();
  return (!input || input.length === 0)
}
