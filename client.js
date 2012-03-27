function NowHs (server, port) {

    this.socket = new WebSocket("ws://" + server + ":" + port);

    this.socket.onopen = function () {
	console.log("Socket opened");
    };

    var thisRef = this;

    this.socket.onmessage = function (msg) {

	console.log (msg);
	var msgJSON = JSON.parse(msg.data);

	var functions = msgJSON[0];
	var schemas = msgJSON[1];

	// set up schemas

	// first put in dummies
	thisRef.schemas = {};
	for (var i in schemas) {
	    thisRef.schemas[schemas[i].schemaName] = {};
	}

	// now create the schemas
	for (var i in schemas) {

	    var fields = schemas[i].schemaFields;
	    var schema = thisRef.schemas[schemas[i].schemaName];
	    for (var j in fields) {
		schema[fields[j][0]] = thisRef.createSchemaField (fields[j][1]);
	    }
	}

	// now for the functions

	// first set up schemafields for the arguments
	thisRef.funcSchFields = {}
	for (var i in functions) {
	    thisRef.funcSchFields[functions[i].functionName] = []
	    for (var j in functions[i].functionArgTypes) {
		thisRef.funcSchFields[functions[i].functionName][j]
		    = thisRef.createSchemaField (functions[i].functionArgTypes[j]);
	    }
	}

	// now set up wrapper functions
	for (var i in functions) {

	    // using raw name for interface but qualified name internally
	    thisRef[functions[i].functionNameRaw] = function (fun) { // closure needed
		return function () {

		    var schemaFields = thisRef.funcSchFields[fun.functionName];


		    // check arguments
		    if (arguments.length - 1 !== schemaFields.length) {
			throw "Arity mismatch: " + schemaFields.length + " arguments + callback expected";
		    }

		    for (var arg = 0; arg < arguments.length - 1; arg++) {
			jsschema.checkField (schemaFields[arg], arguments[arg]);
		    }

		    var functionCall = {
			funName : fun.functionName,
			funArgs : Array.prototype.slice.call (arguments, 0, arguments.length - 1), // retarded javascript
		    }
		    var jsn = JSON.stringify (functionCall);
		    console.log (jsn);
		    thisRef.socket.send (jsn);
		    // TODO: FIGURE OUT asynchronous calls (generate hash for each fcall?)
		}
	    } (functions[i]);
	}

	thisRef.socket.onmessage = function (msg) {
	    console.log ("Yipiieee: " + msg);
	}

    };

    // returns primitive type or reference to schema
    this.primOrSchema = function (obj) {
	switch (obj.type) {
	case "Prim":
	    return obj.value;
	case "SchemaName":
	    return this.schemas[obj.value];
	default:
	    throw ("Cannot happen" + obj.type);
	}
    };

    this.createSchemaField = function (schFieldDesc) {
	switch (schFieldDesc[0]) {
	case "Required":
	    return jsschema.required (this.primOrSchema (schFieldDesc[1]));
	    break;
	case "Optional":
	    return jsschema.optional (this.primOrSchema (schFieldDesc[1]));
	    break;
	case "Repeated":
	    return jsschema.repeated (this.primOrSchema (schFieldDesc[1]));
	    break;
	default:
	    throw ("Cannot happen: " + schFieldDesc[0]);
	}
    };
}

