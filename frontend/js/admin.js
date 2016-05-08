$(document).ready(main);

function main() {
  $.ajax({
    type: "GET",
    url: "http://127.0.0.1:8083/admin/list-teachers",
    dataType: 'json',
    error:
        function(jqXHR, textStatus, errorThrown) {
            console.log("FUCK YOU YOU FUCKING FUCK!!!");
        },
    success:
      function(result) {
        var teachers = result;
        drawTable(teachers);
        $("#add-teacher-button").on('click', function(e) {
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
              drawRow(justAddedTeacher);
            },
            url: "http://127.0.0.1:8083/admin/register-teacher",
            data: JSON.stringify(newTeacher),
          });
        });
      }
    });
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
