Span = function(el)
    -- Define a table to map class names to colors
    local color_map = {
        ["hfv-shadow"] = "1a3454",
        ["hfv-sky"] = "53c6c6",
        ["hfv-lilac"] = "9691cf",
        ["hfv-grass"] = "3a9f9c",
        ["hfv-berry"] = "b1005f",
        ["hfv-desert"] = "e66b3f"
    }

    -- Iterate over the classes of the Span element
    for _, class in pairs(el.classes) do
        -- Check if the class is one of the custom classes
        if color_map[class] then
            -- Construct the raw OpenXML string to apply the color style
            local openXML = '<w:r><w:rPr><w:color w:val="' .. color_map[class] .. '"/></w:rPr><w:t>' .. pandoc.utils.stringify(el.content) .. '</w:t></w:r>'
            -- Return the modified Span element with the raw OpenXML
            return pandoc.RawInline('openxml', openXML)
        end
    end

    -- Return the Span element unchanged if no custom class is found
    return el
end
