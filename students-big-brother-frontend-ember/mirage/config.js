import  _ from 'lodash/lodash';

// const studs = {
//   data: [{
//     type: 'studentSourceFile',
//     id: 1,
//     attributes: {
//       sid: 1,
//       fname:"John",
//       mname:"",
//       lname:"Lennon",
//       file: "#include <cstdlib>"
//     }
//   },
//   {
//     type: 'studentSourceFile',
//     id: 5,
//     attributes: {
//       sid: 1,
//       fname:"John",
//       mname:"",
//       lname:"Lennon",
//       file: ""
//     }
//   }
//     , { type: 'student-source-file'
//       , id: 2
//       , attributes: { sid: 2
//                     , fname:"Paul"
//                     , mname:""
//                     , lname:"McCartney"
//                     , file: "verycomplexc++code"
//                     }
//       }
//     , { type: 'student-source-file'
//       , id: 3
//       , attributes: { sid: 3
//                     , fname:"George"
//                     , mname:""
//                     , lname:"Harisson"
//                     , file: "verycomplexc++code"
//                     }
//       }
//     , { type: 'student-source-file'
//       , id: 4
//       , attributes: { sid: 4
//                     , fname:"Ringo"
//                     , mname:""
//                     , lname:"Starr"
//                     , file: "verycomplexc++code"
//                     }
//       }
//     ]
// };

const studs = {
  data: [
    {
       sid: 1,
       fname:"John",
       mname:"",
       lname:"Lennon",
       file: "#include <cstdlib>"
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
      }
    }
    return {
      data:
        studs.data.filter(s => s.sid == id)
    }
  });
}
