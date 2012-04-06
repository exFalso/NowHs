function NowHs (server, port, readyCallback) {

	this.socket = new WebSocket("ws://" + server + ":" + port);

	this.socket.onopen = function () {
		console.log("Socket opened");
	};

	var self = this;

	this.currentFunId = 0;

	// TODO deal with int overflow
	this.genFunId = function ()  {
		return self.currentFunId++;
	};

	this.callbacks = {};

	this.socket.onmessage = function (msg) {

		var msgJSON = JSON.parse(msg.data);

		var functions = msgJSON[0];
		var schemas = msgJSON[1];

		// set up schemas

		// first put in dummies
		self.schemas = {};
		for (var i in schemas) {
			self.schemas[schemas[i].schemaName] = {};
		}

		// now create the schemas
		for (var i in schemas) {

			var fields = schemas[i].schemaFields;
			var schema = self.schemas[schemas[i].schemaName];
			for (var j in fields) {
				schema[fields[j][0]] = self.createSchemaField (fields[j][1]);
			}
		}

		// now for the functions

		// first set up schemafields for the arguments
		self.funcSchFields = {}
		for (var i in functions) {
			self.funcSchFields[functions[i].functionName] = []
			for (var j in functions[i].functionArgTypes) {
				self.funcSchFields[functions[i].functionName][j]
					= self.createSchemaField (functions[i].functionArgTypes[j]);
			}
		}

		// now set up wrapper functions
		for (var i in functions) {

			// using raw name for interface but qualified name internally
			self[functions[i].functionNameRaw] = function (fun, schemaFields) { // closure needed
			return function () {

				// check arguments
				if (arguments.length - 1 !== schemaFields.length) {
					throw "Arity mismatch: " + schemaFields.length + " arguments + callback expected";
				}

				for (var arg = 0; arg < arguments.length - 1; arg++) {
					jsschema.checkField (schemaFields[arg], arguments[arg]);
				}

				if (typeof arguments[arguments.length - 1] !== "function") {
					throw "Last argument should be a callback function";
				}

				var functionCall = {
					cFunName : fun.functionName,
					cFunId : self.genFunId (),
					cFunArgs : Array.prototype.slice.call (arguments, 0, arguments.length - 1) // retarded javascript
				};

				console.log("callobj", functionCall);
				var jsn = JSON.stringify ({ ClientFCall: functionCall });

				self.socket.send (jsn);

				// TODO: FIGURE OUT asynchronous calls (generate hash for each fcall?)
				var func = arguments[arguments.length - 1]; // freaking scoped arguments wtf javascript

				self.callbacks[functionCall.cFunId] = func;

				self.socket.onmessage = function (msg) {
					var res = JSON.parse (msg.data);
					// res = {
					//   type: "ServerFunctionReturn"
					//   value: {
					//     sFunId:  [generated with genFunId at call]
					//     sRetVal: [callback data]
					//   }
					// }
					console.log("res", res);
					switch (res.type) {
					case "ServerFunctionReturn":
						self.callbacks[res.value.sFunId](res.value.sRetVal);
						delete self.callbacks[res.value.sFunId];
						break;
					default:
						console.error ("unhandled response type " + res.type);
					}
				};
			}

			} (functions[i], self.funcSchFields[functions[i].functionName]);
		}

		if (typeof readyCallback === "function") {
			readyCallback ();
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

