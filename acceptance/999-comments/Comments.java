/* Beginning */

public class Comments {
    // This is a comment.

    public void someMethod() { }

    // This is a comment
    // spanning multiple lines.

    public void someOtherMethod() { }

    /* This is a block comment. */

    /*
      This is a block comment
      over many lines.
    */

    public void /* block comment embedded in a line */ commentedMethod() { }

    public void/* comment without surrounding spaces*/anotherCommentedMethod() { }

    /**
     * This is some Javadoc.
     * This method does nothing at all.
     */
    public void methodWithJavadoc() { }
}

// End
