package ch06;

/**
 * 第6章: TV番組を表す record
 */
public record TvShow(String title, int start, int end) {

    @Override
    public String toString() {
        if (start == end) {
            return String.format("%s (%d)", title, start);
        }
        return String.format("%s (%d-%d)", title, start, end);
    }
}
