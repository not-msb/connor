pub const Type = union(enum) {
    Function: struct {
        params: []const Type,
        ret: *const Type,
    },
    NoReturn,
    CompInt,
    U8,
    U32,
};
