describe('RPCs: client -> server', function() {
  it('runs add :: Int -> Int -> Int', function () {
    runs(function () {
      this.result = 0;

      var self = this;

      var nowhs = new NowHs("localhost", 8888, function () {

        nowhs.sum_rpc(1, 2, function(res) {
          self.result = res;
        });

      });
    });

    waits(100);

    runs(function () {
      expect(this.result).toEqual(3);
    });
  });
});
