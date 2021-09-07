function run_parser7()
    grammarPath = fullfile("resources","grammar.json");
    lexiconPath = fullfile("resources","lexicon.json");

    p = ParserV7(grammarPath, lexiconPath, 'beam', 8);

    disp("Ready. Pegá 1 oración por línea. Enter vacío para salir.");
    while true
        s = input("> ", "s");
        if isempty(strtrim(s))
            break;
        end
        tree = p.parseSentence(s);
        if strlength(tree) == 0
            disp("(NO-PARSE)");
        else
            disp(tree);
        end
    end
end
