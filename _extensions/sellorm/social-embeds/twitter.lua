
local function ensureHtmlDeps()
    quarto.doc.addHtmlDependency({
        name = 'twitter',
        version = '0.0.1',
        scripts = {
            { 
                path = "",
                attribs = {src="https://platform.twitter.com/widgets.js"},
                afterBody = true
            }
        }
    })
end
   
local function isEmpty(s)
    return s == nil or s == ''
end

function tweet(args, kwargs)
    if quarto.doc.isFormat('html') then
        ensureHtmlDeps()

        if isEmpty(args[1]) then
            status_id = pandoc.utils.stringify(kwargs["id"])
            status_image = pandoc.utils.stringify(kwargs["img"])
        else
            status_id = pandoc.utils.stringify(args[1])
            if isEmpty(args[2]) then
            else
              status_image =  pandoc.utils.stringify(args[2])
            end
              
        end
        

        local request = 'https://publish.twitter.com/oembed?url=https://twitter.com/x/status/' .. status_id
        
        local hide_thread = "true"
        
        request = request .. "&hide_thread=" .. hide_thread
        
        -- print(request)

        local success, mime_type, contents = pcall(pandoc.mediabag.fetch, request)
        
        local handled=false
        -- Assemble the twitter oembed API URL from the user inputs
        if success then
            if string.find(mime_type, "json") ~= nil then
              -- http request returned json (good) rather than html (bad, 404 error)
                local parsed = quarto.json.decode(contents)
                if (parsed.html == nil) then
                    print("Could not find tweet with that tweet id " .. status_id .. "")
                else   
                  handled=true
                  return pandoc.RawBlock('html', parsed.html)
                end                
            end
            if (handled==false) then
              if isEmpty(status_image) then
                print("Could not find tweet image, skipping...")
                return pandoc.Null()
              else
                print("Substituting tweet image ".. status_image .. "")
                -- return pandoc.Null()
                return pandoc.RawBlock('html', '<img src="' .. status_image .. '" alt="Tweet image" style="max-width:550px;"/>')
              end
            end 
        else
        -- pcall failed: network error or Twitter unreachable
            if isEmpty(status_image) then
                print("Could not contact Twitter and no fallback image provided, skipping tweet " .. status_id)
                return pandoc.Null()
            else
                print("Could not contact Twitter, substituting tweet image " .. status_image)
                return pandoc.RawBlock('html', '<img src="' .. status_image .. '" alt="Tweet image" style="max-width:550px;"/>')
            end
        end
    else
        return pandoc.Null()
    end
end
