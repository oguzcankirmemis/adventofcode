pub const Vec2 = struct {
    x: i64,
    y: i64,
};

pub const Index2D = struct {
    col: usize,
    row: usize,

    pub fn neighbours(self: Index2D, height: usize, width: usize, buffer: *[8]Index2D) []const Index2D {
        var i: usize = 0;
        if (self.col > 0 and self.row > 0) {
            buffer[i] = .{
                .col = self.col - 1,
                .row = self.row - 1,
            };
            i += 1;
        }
        if (self.col > 0) {
            buffer[i] = .{
                .col = self.col - 1,
                .row = self.row,
            };
            i += 1;
        }
        if (self.col > 0 and self.row + 1 < width) {
            buffer[i] = .{
                .col = self.col - 1,
                .row = self.row + 1,
            };
            i += 1;
        }
        if (self.row > 0) {
            buffer[i] = .{
                .col = self.col,
                .row = self.row - 1,
            };
            i += 1;
        }
        if (self.row + 1 < width) {
            buffer[i] = .{
                .col = self.col,
                .row = self.row + 1,
            };
            i += 1;
        }
        if (self.col + 1 < height and self.row > 0) {
            buffer[i] = .{
                .col = self.col + 1,
                .row = self.row - 1,
            };
            i += 1;
        }
        if (self.col + 1 < height) {
            buffer[i] = .{
                .col = self.col + 1,
                .row = self.row,
            };
            i += 1;
        }
        if (self.col + 1 < height and self.row + 1 < width) {
            buffer[i] = .{
                .col = self.col + 1,
                .row = self.row + 1,
            };
            i += 1;
        }
        return buffer[0..i];
    }
};
