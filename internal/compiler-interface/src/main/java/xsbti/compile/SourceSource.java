package xsbti.compile;


import java.nio.file.Path;


public interface SourceSource {
    public boolean available();
    public String apply(Path path);

    public static final SourceSource empty = new SourceSource() {
        @Override
        public boolean available() {
            return false;
        }

        @Override
        public String apply(Path path) {
            return null;
        }
    };

}
