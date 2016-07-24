import  _ from 'lodash/lodash';

const studs = {
  data: [{
    type: 'studentSourceFile',
    id: 1,
    attributes: {
      sid: 1,
      fname:"John",
      mname:"",
      lname:"Lennon",
      file: "main :: IO ()\
             main = print \"Hello World!\""
    }
  },
  {
    type: 'studentSourceFile',
    id: 5,
    attributes: {
      sid: 1,
      fname:"John",
      mname:"",
      lname:"Lennon",
      file: "main :: IO ()\
             main = print \"Hello World!\""
    }
  }
    , { type: 'student-source-file'
      , id: 2
      , attributes: { sid: 2
                    , fname:"Paul"
                    , mname:""
                    , lname:"McCartney"
                    , file: "verycomplexc++code"
                    }
      }
    , { type: 'student-source-file'
      , id: 3
      , attributes: { sid: 3
                    , fname:"George"
                    , mname:""
                    , lname:"Harisson"
                    , file: "verycomplexc++code"
                    }
      }
    , { type: 'student-source-file'
      , id: 4
      , attributes: { sid: 4
                    , fname:"Ringo"
                    , mname:""
                    , lname:"Starr"
                    , file: "verycomplexc++code"
                    }
      }
    ]
};

export default function() {
  this.get('/student-source-files', function(db, request) {
    let id = request.queryParams.sid;
    if (id === undefined) {
      return {
        data: _.uniq(studs.data, 'attributes.sid')
      }
    }
    return {
      data: studs.data.filter(s => s.attributes.sid == id)
    }
  });
}
