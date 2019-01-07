local txd = engineLoadTXD("rectangle.txd")
engineImportTXD(txd,3458)
---------Speed UP
local mathFloor = math.floor
local mathHuge = math.huge
local mathFrexp = math.frexp

local strGmatch = string.gmatch
local strFormat = string.format
local strSub = string.sub
local strGsub = string.gsub
local strByte = string.byte
local strChar = string.char
local strFind = string.find
local tabInsert = table.insert
local strSplit = function(str, delimiter)
	if str==nil or str=='' or delimiter==nil then
		return nil
	end
	
    local result = {}
    for match in (str):gmatch("(.-)"..delimiter) do
        tabInsert(result, match)
    end
    return result
end
--Hex To Float
local Hex2Float = function (c)
	if c == 0 then return 0.0 end
	local b1,b2,b3,b4 = 0,0,0,0
	b1 = c/0x1000000
	b1 = b1-b1%1
	c = c - b1*0x1000000
	b2 = c/0x10000
	b2 = b2-b2%1
	c = c - b2*0x10000
	b3 = c/0x100
	b3 = b3-b3%1
	c = c - b3*0x100
	b4 = c
	b4 = b4-b4%1
	local sign,temp = b1 > 0x7F, b2 / 0x80
	local expo = b1 % 0x80 * 0x2 + temp-temp%1
	local mant = (b2 % 0x80 * 0x100 + b3) * 0x100 + b4
	if sign then
		sign = -1
	else
		sign = 1
	end
	local n
	if mant == 0 and expo == 0 then
		n = sign * 0.0
	elseif expo == 0xFF then
		if mant == 0 then
			n = sign * mathHuge
		else
			n = 0.0/0.0
		end
	else
		n = sign*(1.0+mant/0x800000)*2^(expo-0x7F)
	end
	return n
end

function intToData(int,leng,rev)
	if leng then
		local str = ""
		while true do
			local value = int/256
			local temp = value-value%1
			local theRest = int-temp*256
			int = temp
			str = str..strChar(theRest)
			if leng == #str then
				return str
			end
		end
	else
		local str = ""
		while true do
			local value = int/256
			local temp = value-value%1
			local theRest = int-temp*256
			int = temp
			str = str..strChar(theRest)
			if int == 0 then
				return str
			end
		end
	end
end

function dataToInt(str,rev)
	local num = 0
	if not rev then
		local len = 0
		for value in strGmatch(str,".") do
			num = num + strByte(value)*256^len
			len=len+1
		end
		return num
	else
		local len = #str
		for value in strGmatch(str,".") do
			len=len-1
			num = num + strByte(value)*256^len
		end
		return num
	end
end

local Float2Hex = function(n)
	if n == 0 then return 0 end
	local sign = 0
	if n < 0 then
		sign = 0x80
		n = -n
	end
	local mant, expo = mathFrexp(n)
	local hext1,hext2,hext3,hext4
	if mant ~= mant then
		hext1 = 0xFF
		hext2 = 0x88
		hext3 = 0x00
		hext4 = 0x00
	elseif mant == mathHuge or expo > 0x80 then
		hext2 = 0x80
		hext3 = 0x00
		hext4 = 0x00
		if sign == 0 then
			hext1 = 0x7F
		else
			hext1 = 0xFF
		end
	elseif (mant == 0.0 and expo == 0) or expo < -0x7E then
		hext1 = sign
		hext2 = 0x00
		hext3 = 0x00
		hext4 = 0x00
	else
		expo = expo + 0x7E
		mant = (mant*2.0-1.0)*0x800000
		local temp1 = expo/0x2
		temp1=temp1-temp1%1
		local temp2 = mant/0x10000
		temp2=temp2-temp2%1
		local temp3 = mant/0x100
		temp3=temp3-temp3%1
		hext1 = sign + temp1
		hext2 = expo%0x2*0x80+temp2
		hext3 = temp3%0x100
		hext4 = mant%0x100
	end
	return mathFloor(hext1*0x1000000+hext2*0x10000+hext3*0x100+hext4)
end
----------------

DFF = {}
DFF.Header = 0x00
----------In Header
DFF.ObjectCountOffset = {0x19,4}
DFF.SectionSizeOffset = {0x5,4}
DFF.RWVersionOffset = {0x9,4}
DFF.SectionTypeOffset = {0x1,4}
----------In Section
----------
DFF.RWVersion = {
	[0x0C02FFFF] = "GTA VC",
	[0x0800FFFF] = "GTA III",
	[0x1803FFFF] = "GTA SA",
}

DFF.SectionType = {
	[0x00000001] = "Struct",
	[0x00000002] = "String",
	[0x00000003] = "Extension",
	[0x00000006] = "Texture",
	[0x00000007] = "Material",
	[0x00000008] = "Material List",
	[0x0000000E] = "Frame List",
	[0x0000000F] = "Geometry",
	[0x00000010] = "Clump",
	[0x00000014] = "Atomic",
	[0x0000001A] = "Geometry List",
	[0x0000050E] = "Material Split",
	[0x0253F2F3] = "Pipeline Set",
	[0x0253F2F6] = "Specular Material",
	[0x0253F2F8] = "2DFX",
	[0x0253F2F9] = "Night Vertex Color",
	[0x0253F2FA] = "Collision Model",
	[0x0253F2FC] = "Reflection Material",
	[0x0253F2FD] = "Mesh Extension",
	[0x0253F2FE] = "Frame",
}
--------------------------Reader
----------------MetaTable
local geometryVertexMeta = {
	setPosition = function(self,x,y,z,withoutGroup)
		assert(type(x) == "number","Bad Argument @setPosition at argument 1, expect a float got "..type(x))
		assert(type(y) == "number","Bad Argument @setPosition at argument 2, expect a float got "..type(y))
		assert(type(z) == "number","Bad Argument @setPosition at argument 3, expect a float got "..type(z))
		local xData = intToData(Float2Hex(x),4)
		local yData = intToData(Float2Hex(y),4)
		local zData = intToData(Float2Hex(z),4)
		local moveTable = {}
		if not withoutGroup then
			moveTable = self.parent.weldGroup[self.id] or {self}
		else
			moveTable = {self}
		end
		for k,v in ipairs(moveTable) do
			local vertexOffset = v[0]
			v.parent[2] = v.parent[2]:sub(1,vertexOffset-1)..xData..yData..zData..v.parent[2]:sub(vertexOffset+0x0C)
			v[1] = x
			v[2] = y
			v[3] = z
		end
		return true
	end,
	moveOffset = function(self,x,y,z,withoutGroup)
		assert(type(x) == "number","Bad Argument @setPosition at argument 1, expect a float got "..type(x))
		assert(type(y) == "number","Bad Argument @setPosition at argument 2, expect a float got "..type(y))
		assert(type(z) == "number","Bad Argument @setPosition at argument 3, expect a float got "..type(z))

		local moveTable = {}
		if not withoutGroup then
			moveTable = self.parent.weldGroup[self.id] or {self}
		else
			moveTable = {self}
		end
		for k,v in ipairs(moveTable) do
			local vertexOffset = v[0]
			local xData = intToData(Float2Hex(x+self[1]),4)
			local yData = intToData(Float2Hex(y+self[2]),4)
			local zData = intToData(Float2Hex(z+self[3]),4)
			v.parent[2] = v.parent[2]:sub(1,vertexOffset-1)..xData..yData..zData..v.parent[2]:sub(vertexOffset+0x0C)
			v[1] = x
			v[2] = y
			v[3] = z
		end
		return true
	end,
}

local g_GeometryVertexMeta = {
	weldVertex = function(self)
		local mapTable = {}
		self.parent.weldGroup = {}
		for i=1,#self do
			local x,y,z = self[i][1],self[i][2],self[i][3]
			local str = x.."-"..y.."-"..z
			mapTable[str] = mapTable[str] or {}
			mapTable[str][#mapTable[str]+1] = self[i]
			self.parent.weldGroup[i] = mapTable[str]
		end
	end,
}

local geometryVertexColorMeta = {
	setColor = function(self,r,g,b,a,withoutGroup)
		assert(type(r) == "number","Bad Argument @setColor at argument 1, expect a float got "..type(r))
		assert(type(g) == "number","Bad Argument @setColor at argument 2, expect a float got "..type(g))
		assert(type(b) == "number","Bad Argument @setColor at argument 3, expect a float got "..type(b))
		assert(type(a) == "number","Bad Argument @setColor at argument 4, expect a float got "..type(a))
		local rData = intToData(r)
		local gData = intToData(g)
		local bData = intToData(b)
		local aData = intToData(a)
		local moveTable = {}
		if not withoutGroup then
			moveTable = self.parent.weldGroup[self.id] or {self}
		else
			moveTable = {self}
		end
		for k,v in ipairs(moveTable) do
			local vertexColorOffset = self.parent.GeometryData.vertexColor[v.id][0]
			v.parent[2] = v.parent[2]:sub(1,vertexColorOffset-1)..rData..gData..bData..aData..v.parent[2]:sub(vertexColorOffset+0x04)
			v[1] = r
			v[2] = g
			v[3] = b
			v[4] = a
		end
		return true
	end,
}

local geometryUVMeta = {
	setUVPosition = function(self,x,y,z)
		assert(type(x) == "number","Bad Argument @setUVPosition at argument 1, expect a float got "..type(x))
		assert(type(y) == "number","Bad Argument @setUVPosition at argument 2, expect a float got "..type(y))
		local xData = intToData(Float2Hex(x))
		local yData = intToData(Float2Hex(y))
		local textureOffset = self[0]
		self.parent[2] = self.parent[2]:sub(1,textureOffset-1)..xData..yData..self.parent[2]:sub(textureOffset+0x03)
		self[1] = x
		self[2] = y
		return true
	end,
}

local frameListRotationMeta = {
	setRotation = function(rx,ry,rz)
		
	end,
	getRotation = function()
	
	end,
}

----------------
function readGeometry(struct,skipTexture,skipPerlit,skipNormals,skipFace,skipVertex)
	local GeometryData = {}
	local rawData = struct[2]
	local flag = strRead(rawData,0x01,0x04)
	local GeometryTristrip = flag%2
	flag = (flag-GeometryTristrip)/2
	local GeometryPositions = flag%2
	flag = (flag-GeometryPositions)/2
	local GeometryTextured = flag%2
	flag = (flag-GeometryTextured)/2
	local GeometryPrelit = flag%2
	flag = (flag-GeometryPrelit)/2
	local GeometryNormals = flag%2
	flag = (flag-GeometryNormals)/2
	local GeometryLight = flag%2
	flag = (flag-GeometryLight)/2
	local GeometryModulateMaterialColor = flag%2
	flag = (flag-GeometryModulateMaterialColor)/2
	local GeometryTextured2 = flag%2
	flag = (flag-GeometryTextured2)/2
	local faceCount = strRead(rawData,0x05,0x04)
	local vertexCount = strRead(rawData,0x09,0x04)
	local frameCount = strRead(rawData,0x0D,0x04)
	local offset = 0x11	--17
	local vertexColor = {parent=struct,GeometryData=GeometryData}
	if GeometryPrelit == 1 then
		if not skipPerlit then
			for i=1,vertexCount do
				local vertexOffset = offset
				local r = strRead(rawData,offset,0x01)
				offset = offset+0x01
				local g = strRead(rawData,offset,0x01)
				offset = offset+0x01
				local b = strRead(rawData,offset,0x01)
				offset = offset+0x01
				local a = strRead(rawData,offset,0x01)
				offset = offset+0x01
				vertexColor[i] = {id=i,parent=struct,[0]=vertexOffset,r,g,b,a}
				setmetatable(vertexColor[i],{__index=geometryVertexColorMeta})
			end
		else
			offset = offset+0x04*vertexCount
		end
	end
	local uv = {parent=struct,GeometryData=GeometryData}
	if GeometryTextured == 1 then
		if not skipTexture then
			for i=1,vertexCount do
				local textureOffset = offset
				local u = Hex2Float(strRead(rawData,offset,0x04))
				offset = offset+0x04
				local v = Hex2Float(strRead(rawData,offset,0x04))
				offset = offset+0x04
				uv[i] = {id=i,parent=struct,[0]=textureOffset,u,v}
				setmetatable(uv[i],{__index=geometryVertexColorMeta})
			end
		else
			offset = offset+0x08*vertexCount
		end
	end
	local faces = {parent=struct,GeometryData=GeometryData}
	if not skipFace then
		for i=1,faceCount do
			local faceB = strRead(rawData,offset,0x02)
			offset = offset+0x02
			local faceA = strRead(rawData,offset,0x02)
			offset = offset+0x02
			local faceF = strRead(rawData,offset,0x02)
			offset = offset+0x02
			local faceC = strRead(rawData,offset,0x02)
			offset = offset+0x02
			faces[i] = {id=i,parent=struct,faceB,faceA,faceF,faceC}
		end
	else
		offset = offset+0x08*vertexCount
	end
	local boundSphereX = Hex2Float(strRead(rawData,offset,0x04))
	offset = offset+0x04
	local boundSphereY = Hex2Float(strRead(rawData,offset,0x04))
	offset = offset+0x04
	local boundSphereZ = Hex2Float(strRead(rawData,offset,0x04))
	offset = offset+0x04
	local boundSphereW = Hex2Float(strRead(rawData,offset,0x04))
	offset = offset+0x04
	local boundSphere = {boundSphereX,boundSphereY,boundSphereZ,boundSphereW}
	offset = offset+0x04	--Unknown
	offset = offset+0x04	--Unknown
	local vertex = {parent=struct,GeometryData=GeometryData}
	if not skipVertex then
		for i=1,vertexCount do
			local vertexOffset = offset
			local x = Hex2Float(strRead(rawData,offset,0x04))
			offset = offset+0x04
			local y = Hex2Float(strRead(rawData,offset,0x04))
			offset = offset+0x04
			local z = Hex2Float(strRead(rawData,offset,0x04))
			offset = offset+0x04
			vertex[i] = {id=i,parent=struct,[0]=vertexOffset,x,y,z}
			setmetatable(vertex[i],{__index=geometryVertexMeta})
		end
		setmetatable(vertex,{__index=g_GeometryVertexMeta})
	else
		offset = offset+0x0C*vertexCount
	end
	local normal = {parent=struct,GeometryData=GeometryData}
	if GeometryNormals == 1 then
		if not skipNormals then
			for i=1,vertexCount do
				local vertexOffset = offset
				local x = Hex2Float(strRead(rawData,offset,0x04))
				offset = offset+0x04
				local y = Hex2Float(strRead(rawData,offset,0x04))
				offset = offset+0x04
				local z = Hex2Float(strRead(rawData,offset,0x04))
				offset = offset+0x04
				normal[i] = {id=i,parent=struct,[0]=vertexOffset,x,y,z}
			end
		end
		offset = offset+0x0C*vertexCount
	end
	GeometryData.GeometryTristrip = GeometryTristrip == 1 and true or false
	GeometryData.GeometryPositions = GeometryPositions == 1 and true or false
	GeometryData.GeometryTextured = GeometryTextured == 1 and true or false
	GeometryData.GeometryPrelit = GeometryPrelit == 1 and true or false
	GeometryData.GeometryNormals = GeometryNormals == 1 and true or false
	GeometryData.GeometryLight = GeometryLight == 1 and true or false
	GeometryData.GeometryModulateMaterialColor = GeometryModulateMaterialColor == 1 and true or false
	GeometryData.GeometryTextured2 = GeometryTextured2 == 1 and true or false
	GeometryData.faceCount = faceCount
	GeometryData.vertexCount = vertexCount
	GeometryData.frameCount = frameCount
	GeometryData.face = faces
	GeometryData.vertex = vertex
	GeometryData.vertexColor = vertexColor
	GeometryData.normal = normal
	GeometryData.uv = uv
	struct.GeometryData = GeometryData
	return GeometryData
end

function readFrameList(struct)
	local rawData = struct[2]
	local frameCount = strRead(rawData,0x01,0x04)
	local frames = {}
	local offset = 0x05
	for i=1,frameCount do
		frames[i] = frames[i] or {}
		frames[i].rotationMatrix = {{},{},{}}
		for line=1,3 do
			for list=1,3 do
				frames[i].rotationMatrix[line][list] = Hex2Float(strRead(rawData,offset,offset+0x04))
				offset = offset+0x04
			end
		end
		local posX = Hex2Float(strRead(rawData,offset,offset+0x04))
		offset = offset+0x04
		local posY = Hex2Float(strRead(rawData,offset,offset+0x04))
		offset = offset+0x04
		local posZ = Hex2Float(strRead(rawData,offset,offset+0x04))
		offset = offset+0x04
		frames[i].position = {posX,posY,posZ}
		local parent = strRead(rawData,offset,offset+0x04)
		frames[i].parentFrame = parent ~= 0xFFFFFFFF and parent
		offset = offset+0x04
		frames[i].integer = strRead(rawData,offset,offset+0x04)
	end
	return frames
end

--------------------------Metatable
sectionMeta = {
	getType = function(self)
		local offset = DFF.SectionTypeOffset
		local sectionType = strRead(self[1],offset[1],offset[2])
		return DFF.SectionType[sectionType] or "Unknown"
	end,
	getSize = function(self)
		local offset = DFF.SectionSizeOffset
		local sectionSize = strRead(self[1],offset[1],offset[2])
		return sectionSize
	end,
	read = function(self)
		local sectionType = self:getType()
		if sectionType == DFF.SectionType[0x0000000F] then	--Geometry
			local parent = self.parent
			local myindex = self.index
			local struct = parent[myindex+1]
			return readGeometry(struct)
		elseif sectionType == DFF.SectionType[0x0000000E] then	--Frame List
			local parent = self.parent
			local myindex = self.index
			local struct = parent[myindex+1]
			return readFrameList(struct)
		end
	end,
}

function strRead(str,idStart,size,rev)
	local idStart = idStart or 1
	local size = size or 0
	local data = strSub(str,idStart,size+idStart-1)
	return dataToInt(data,rev),data
end

function getDFFRWVersion(str)
	local rwVer = DFF.RWVersionOffset
	local data,raw = strRead(str,rwVer[1],rwVer[2])
	return DFF.RWVersion[data],raw
end

function getDFFObjectCount(str)
	local objCnt = DFF.ObjectCountOffset
	return strRead(str,objCnt[1],objCnt[2])
end

function getDFFSize(str)	--Bytes
	local objCnt = DFF.SectionSizeOffset
	return strRead(str,objCnt[1],objCnt[2])
end

--Offset includes identifier
function getDFFSections(str,identifierText)
	local sectionTable = {}
	local cnt = 1
	local offset = strFind(str,identifierText,cnt,true)
	while true do
		currentPos = strFind(str,identifierText,offset+1,true)
		if not currentPos then
			sectionTable[cnt] = {parent=sectionTable,index=cnt,[0]=offset,strSub(str,offset-8,offset-1),strSub(str,offset+4)}
			setmetatable(sectionTable[cnt],{__index=sectionMeta})
			break
		else
			sectionTable[cnt] = {parent=sectionTable,index=cnt,[0]=offset,strSub(str,offset-8,offset-1),strSub(str,offset+4,currentPos-9)}
			setmetatable(sectionTable[cnt],{__index=sectionMeta})
		end
		cnt = cnt+1
		offset = currentPos
	end
	return sectionTable
end

function getSectionHeaderData(sectionHeadRaw)
	local offset = DFF.SectionTypeOffset
	local sectionType = strRead(sectionHeadRaw,offset[1],offset[2])
	local offset = DFF.SectionSizeOffset
	local sectionSize = strRead(sectionHeadRaw,offset[1],offset[2])
	return DFF.SectionType[sectionType] or "Unknown",sectionSize
end

--function readStruct
---------------------------

function loadDFF(file)
	local file = fileOpen(file)
	local str = fileRead(file,fileGetSize(file))
	fileClose(file)
	local DFFTable = {rawData=str}
	DFFTable.dffElement = engineLoadDFF(str)
	DFFTable.objectCount = getDFFObjectCount(str)
	DFFTable.size = getDFFSize(str)
	DFFTable.RWVersion,DFFTable.identifierText = getDFFRWVersion(str)
	DFFTable.sectionRawData = getDFFSections(str,DFFTable.identifierText)
	DFFTable.sectionRawData.parent = DFFTable
	DFFTable.findSection = function(self,sectionType,id)
		local sections = self.sectionRawData
		local cnt = 0
		local id = id or 1
		for i=1,#sections do
			local secType = sections[i]:getType()
			if secType == sectionType then
				cnt = cnt + 1
				if cnt == id then
					return sections[i],i
				end
			end
		end
		return false
	end
	DFFTable.packRawData = function(self)
		local data = ""
		for i=1,#self.sectionRawData do
			data = data..self.sectionRawData[i][1]..DFFTable.identifierText..self.sectionRawData[i][2]
		end
		self.rawData = data
	end
	DFFTable.reloadElement = function(self)
		local newDFF = engineLoadDFF(self.rawData)
		assert(isElement(newDFF),"Can not load dff")
		if isElement(self.dffElement) then
			destroyElement(self.dffElement)
		end
		self.dffElement = newDFF
	end
	DFFTable.replaceModel = function(self,id)
		engineReplaceModel(self.dffElement,id)
	end
	return DFFTable
end

--Test

obj = createObject(3458,0,0,2.5)
local DFF = loadDFF("rectangle.dff")
local framelist = DFF:findSection("Geometry"):read()
framelist.vertex:weldVertex()
framelist.vertex[510]:moveOffset(0,0,1)
for i=1,1600 do
	framelist.vertexColor[i]:setColor(150,250,10,255,true)
end
DFF:packRawData()
DFF:reloadElement()
DFF:replaceModel(3458)