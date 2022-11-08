import asyncio
import websockets
import sys
import json
import threading


class WebsocketBridge:
    def __init__(self, appName, emacsPort, messageHandler):
        self.appName = appName
        self.emacsPort = emacsPort
        self.messageHandler = messageHandler
        
    async def start(self):
        await self.ws_messageHandler(self.messageHandler)

    async def ws_messageHandler(self, messageHandler):
        async for websocket in websockets.connect(
                "ws://localhost:" + self.emacsPort):
            self.client = websocket
            await websocket.send(
                json.dumps({"type": "client-app-name", "content": self.appName}))
            async for message in websocket:
                await messageHandler(message)

    async def messageToEmacs(self, message):
        await self.client.send(json.dumps({"type": "show-message", "content": message}))

    async def evalInEmacs(self, code):
        await self.client.send(json.dumps({"type": "eval-code", "content": code}))
        
    async def getEmacsVar(self, varName):
        async for websocket in websockets.connect("ws://localhost:" + self.emacsPort):
            await websocket.send(json.dumps({"type": "fetch-var", "content": varName}))
            async for message in websocket:
                return message['data']


