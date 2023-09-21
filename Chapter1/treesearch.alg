BEGIN
    INTEGER ARRAY Data[100];      COMMENT Array to hold node values
    INTEGER ARRAY Left[100];      COMMENT Array to hold left child indices
    INTEGER ARRAY Right[100];     COMMENT Array to hold right child indices

    BOOLEAN PROCEDURE Treesearch(root, arg, m);
    VALUE root, arg, m; INTEGER root, arg, m;
    BEGIN
        COMMENT Data[], Left[], Right are global arrays in this procedure.
        This procedure searches the tree with root 'root' for the node n such that Data[n] == arg.
        IF root = 0 THEN
            Treesearch := FALSE
        ELSE
            BEGIN
                INTEGER d;
                d := Data[root];
                IF arg = d THEN
                    BEGIN
                        m := root;
                        Treesearch := TRUE;
                    END
                ELSE
                    IF arg > d THEN
                        Treesearch := Treesearch(Left[root], arg, m)
                    ELSE
                        Treesearch := Treesearch(Right[root], arg, m);
            END;
    END;

    COMMENT Build the binary tree structure (example):
    Data[1] := 10;               COMMENT Root node with value 10
    Left[1] := 2;                COMMENT Index of left child
    Right[1] := 3;               COMMENT Index of right child

    Data[2] := 5;                COMMENT Left child of root with value 5
    Left[2] := 4;
    Right[2] := 0;               COMMENT No right child

    Data[3] := 15;               COMMENT Right child of root with value 15
    Left[3] := 0;                COMMENT No left child
    Right[3] := 6;               COMMENT Index of right child

    INTEGER result_index;
    BOOLEAN found;
    found := Treesearch(1, 15, result_index);  COMMENT Search for value 15 in the tree

    IF found THEN
        BEGIN
            OUTSTRING("Value 15 found at index: ");
            OUTINT(result_index);
        END
    ELSE
        OUTSTRING("Value 15 not found in the tree.");
END.