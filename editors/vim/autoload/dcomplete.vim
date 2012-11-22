"The completion function
function! dcomplete#Complete(findstart,base)
	if a:findstart
		let prePos=searchpos('\W',"bn")
		let preChar=getline(prePos[0])[prePos[1]-1]
		if '.'==preChar
			"Save the data for use in the dscanner call.
			let b:dscanner_lastPos=prePos
			let b:dscanner_type='.'
			return prePos[1]
		endif
		"If we can't find a dot, we look for a paren.
		let prePos=searchpos("(","bn",line('.'))
		if prePos[0]
			if getline('.')[prePos[1]:col('.')-2]=~'^\s*\w*$'
				let b:dscanner_lastPos=prePos
				let b:dscanner_type='('
				return prePos[1]
			endif
		endif
		"If we can't find either, dscanner can't help us.
		return -2
	else
		"Run dscanner according to the preChar.
		if b:dscanner_type=='.'
			let scanResult=s:runDScanner('dotComplete')
		else
			let scanResult=s:runDScanner('parenComplete')
		endif
		"Split the result text to lines.
		let resultLines=split(scanResult,"\n")
		"If the last line is --------------, we have an error.
		if resultLines[-1]=~'-\+'
			return resultLines[-1]
		end

		"Identify the type of result received
		let b:dscanner_resultType=resultLines[0]
		"Parse the result accoring to their type
		if b:dscanner_resultType=='dotComplete'
			return s:parsePairs(a:base,resultLines,'','')
		elseif b:dscanner_resultType=='completions'
			return s:parsePairs(a:base,resultLines,'',')')
		elseif b:dscanner_resultType=='calltips'
			return s:parseCalltips(a:base,resultLines)
		endif
	endif
endfunction

"Run dscanner
function! s:runDScanner(scanCommand)
	if exists('g:dscanner_path')
		let l:dscannerCommand=g:dscanner_path
	else
		let l:dscannerCommand='dscanner'
	endif

	if exists('g:dscanner_includePath')
		let l:dscannerCommand=l:dscannerCommand.' -I'.g:dscanner_includePath
	endif


	let l:tmpFileName=tempname()
	exec "write ".l:tmpFileName
	let scanResult=system(l:dscannerCommand.' --'.a:scanCommand.' '.(line2byte('.')+col('.')-2).' <'.l:tmpFileName)
	call delete(l:tmpFileName)
	return scanResult
endfunction

"Parse simple pair results
function! s:parsePairs(base,resultLines,addBefore,addAfter)
	let result=[]
	for resultLine in a:resultLines[1:]
		if len(resultLine)
			let lineParts=split(resultLine)
			if lineParts[0]=~'^'.a:base
				call add(result,{'word':a:addBefore.lineParts[0].a:addAfter,'kind':lineParts[1]})
			endif
		end
	endfor
	return result
endfunction

"Parse function calltips results
function! s:parseCalltips(base,resultLines)
	let result=[a:base]
	for resultLine in a:resultLines[1:]
		if len(resultLine)
			let funcArgs=[]
			for funcArg in split(resultLine[match(resultLine,'(')+1:-2],',\\n\\t')
				let argParts=split(funcArg)
				if 1<len(argParts)
					call add(funcArgs,argParts[-1])
				else
					call add(funcArgs,'')
				endif
			endfor
			let funcArgsString=join(funcArgs,',').')'
			call add(result,{'word':funcArgsString,'abbr':substitute(resultLine,'\\n\\t','','g'),'dup':1})
		end
	endfor
	return result
endfunction
