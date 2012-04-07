window.newNowHs = newNowHs = (callback) ->
  new NowHs "localhost", 8888, callback


returns = (call, expected, timeout = 10) ->
  runs ->
    @result = "not set"
    call (res) => @result = res

  waits timeout

  runs ->
    expect(@result).toEqual expected


describe "RPCs: client -> server", ->

  nowhs = null

  it "connects via WebSocket", ->
    runs -> nowhs = newNowHs()
    waits 50
    runs -> expect(nowhs.socket.readyState).toEqual 1

  it "runs add :: Int -> Int -> Int", ->
    returns ((res) -> nowhs.sum_rpc 1, 2, res), 3

  it "accepts composite data structures: authenticated :: User -> Bool", ->
    returns ((res) -> nowhs.authenticated_rpc { name: 'John', age: 17 }, res), false
    returns ((res) -> nowhs.authenticated_rpc { name: 'Jack', age: 18 }, res), true

  it "returns composite data structures: firstUser :: User", ->
    returns ((res) -> nowhs.firstUser_rpc res), { name: 'John', age: 17 }

  # Don't expect this yet - see http://code.google.com/p/phantomjs/issues/detail?id=486
  # it "can close the WebSocket", ->
  #   runs -> nowhs.socket.close()
  #   waits 100
  #   runs -> expect(nowhs.socket.readyState).toEqual 3
