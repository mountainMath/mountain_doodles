local function isEmpty(s)
    return s == nil or s == ''
end

function youtube(args,kwargs)
    if quarto.doc.isFormat('html') then
        local video_id = ""
        if isEmpty(args[1]) then
            videoid = pandoc.utils.stringify(kwargs["id"])
        else
            videoid = pandoc.utils.stringify(args[1])
        end


        -- Assemble HTML to be returned
        local html = '<div id="youtube-frame" style="position: relative; padding-bottom: 56.25%; /* 16:9 */ height: 0;"><iframe width="100%" height="" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;" src="https://www.youtube.com/embed/'
            .. videoid
            .. '" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div>'

	return pandoc.RawInline('html', html)
    else
        return pandoc.Null()
    end
end
