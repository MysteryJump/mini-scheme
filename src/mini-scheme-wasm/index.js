const rust = import("./pkg");

const executeLines = async (lines) => {
    try {
        const m = await rust;
        return m.execute_lines(lines);
    } catch (err) {
        return err;
    }
};


