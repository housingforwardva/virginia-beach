Span = function(el)
    -- Define a table to map class names to colors
    local color_map = {
        ["hfv-shadow"] = "1a3454",
        ["hfv-sky"] = "53c6c6",
        ["hfv-lilac"] = "9691cf",
        ["hfv-grass"] = "3a9f9c",
        ["hfv-berry"] = "B1005F",
        ["hfv-desert"] = "e66b3f"
    }

    -- Define a table to check for bold formatting
    local bold_classes = {
        ["hfv-shadow-bold"] = true,
        ["hfv-sky-bold"] = true,
        ["hfv-lilac-bold"] = true,
        ["hfv-grass-bold"] = true,
        ["hfv-berry-bold"] = true,
        ["hfv-desert-bold"] = true
    }

    -- Iterate over the classes of the Span element
    for _, class in pairs(el.classes) do
        -- Check if the class is one of the custom classes
        if color_map[class] then
            -- Construct the raw OpenXML string to apply the color style
            local openXML = '<w:r><w:rPr><w:color w:val="' .. color_map[class] .. '"/></w:rPr><w:t>' .. pandoc.utils.stringify(el.content) .. '</w:t></w:r>'
            -- Return the modified Span element with the raw OpenXML
            return pandoc.RawInline('openxml', openXML)
        -- Check if the class is one of the bold classes
        elseif bold_classes[class] then
            -- Extract the base class name to get the color
            local base_class = string.match(class, "^(.-)-bold$")
            -- Construct the raw OpenXML string to apply the color style and bold formatting
            local openXML = '<w:r><w:rPr><w:color w:val="' .. color_map[base_class] .. '"/><w:b/></w:rPr><w:t>' .. pandoc.utils.stringify(el.content) .. '</w:t></w:r>'
            -- Return the modified Span element with the raw OpenXML
            return pandoc.RawInline('openxml', openXML)
        end
    end

    -- Return the Span element unchanged if no custom class is found
    return el
end
