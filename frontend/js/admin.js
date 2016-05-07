$(document).ready(function() {
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
          var newTeacher = {username:"lal1",password:"lal2"};
          console.log(newTeacher);
          $.ajax({
            type: "POST",
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            url: "http://127.0.0.1:8083/admin/register-teacher",
            data: JSON.stringify(newTeacher),
            success: function(data, textStatus, jqXHR) {
              console.log("lalki");
            },
            error: function(jqXHR, textStatus, errorThrown) {
              console.log(errorThrown);
            }
          });
        });
      }
    });
});

function drawTable(data) {
    for (var i = 0; i < data.length; ++i) {
        drawRow(data[i]);
    }

}

function drawRow(rowData) {
    var row = $("<tr />")
    $("#teachers-list-table").append(row); //this will append tr element to table... keep its reference for a while since we will add cels into it
    row.append($("<td>" + rowData.teacher_id + "</td>"));
    row.append($("<td>" + rowData.teacher_credential.username + "</td>"));
    row.append($("<td>" + rowData.teacher_credential.password + "</td>"));
    row.append(
      $("<td><button class=\"teacher-delete-button\">Delete</button></td>")
    );
}
