import  _ from 'lodash/lodash';

const studs = {
  data: [
    {
       sid: 1,
       fname:"John",
       mname:"",
       lname:"Lennon",
       file:
"#include <cstdlib> \nusing namespace std;\nint main() {}"
    },
    {
      sid: 1,
      fname:"John",
      mname:"",
      lname:"Lennon",
      file: ""
    },
    {
      sid: 2,
      fname:"Paul",
      mname:"",
      lname:"McCartney",
      file: ""
    },
    {
      sid: 3,
      fname:"George",
      mname:"",
      lname:"Harisson",
      file: ""
    },
    {
      sid: 4,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 5,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 6,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 7,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 8,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 9,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 10,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 11,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    },
    {
      sid: 12,
      fname:"Ringo",
      mname:"",
      lname:"Starr",
      file: ""
    }
  ]
};

export default function() {
  this.get('/student-source-files', function(db, request) {
    let id = request.queryParams.sid;
    if (id === undefined) {
      return {
        data: _.uniq(studs.data, 'sid')
        // data: studs.data
      };
    }
    return {
      data:
        studs.data.filter(s => s.sid === id)
    };
  });
}
