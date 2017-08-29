#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import socket, struct, hashlib, threading, cgi
import base64, array, io

from time import *
from copy import deepcopy

from StringIO import StringIO # Python 2

from os import listdir
from os.path import isfile, join

from subprocess import Popen, PIPE, STDOUT

class Client:
  def __init__(self, connection):
    self.connection = connection

  def __str__(self):
    return self.connection

clients = []

GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

def create_hash(key):
  reply = key + GUID
  sh1 = hashlib.sha1(reply)
  return sh1.digest()


def recv_data(client):   # Function that is executed when the server receives a message.
  data = client.recv(2)
  head1, head2 = struct.unpack('!BB', data)
  fin = bool(head1 & 0b10000000)
  global opcode
  opcode = head1 & 0b00001111
  length = head2 & 0b01111111
  if length == 126:
    data = client.recv(2)
    length, = struct.unpack('!H', data)
  elif length == 127:
    data = client.recv(8)
    length, = struct.unpack('!Q', data)

  mask_bits = client.recv(4)
  mask_bits = bytearray(mask_bits)
  data = client.recv(length)
  data = bytearray(data)
  DECODED = []
  for i in range(0, len(data)):
    DECODED.append(data[i] ^ mask_bits[i % 4])
  DECODED = array.array('B', DECODED).tostring()

  # The message received is a string stored in DECODED.

  global p, inputfile

  if DECODED == "getinput":
    client.send(write_frame("input" + inputfile, fin=1))
    return
  
  if DECODED == "files":
    files = Popen(["find", "."], stdout=PIPE).communicate()[0].split("\n")
    filtered = [f for f in files if f[-4:] == ".ggm"]
    client.send(write_frame("files" + "@".join(filtered), fin=1))
    return

  if DECODED[:4] == "open":
    try:
      inputfile = ""
      f = open(DECODED[4:], 'r')
      for line in f.readlines():
        inputfile += line
      client.send(write_frame("input" + inputfile, fin=1))
    except:
      client.send(write_frame("input" + "Unknown file", fin=1))
    return

  ocamlinput = DECODED.replace("\n","")
  p.stdin.write(ocamlinput + "\n")
  p.stdin.flush()

  output = p.stdout.readline()

  for line in output.split("@"):
    if "ParserError" in line or "Failure" in line:
      msg = output.replace("@","").replace("\\\\","\\")
      client.send(write_frame("error" + msg, fin=1))
      break
    
    line = line.replace("\n","")
    client.send(write_frame(line, fin=1))


def write_frame(data, opcode=1, fin=0, masking_key=False):
  if fin > 0x1:
    raise ValueError('FIN bit parameter must be 0 or 1')
  if 0x3 <= opcode <= 0x7 or 0xB <= opcode:
    raise ValueError('Opcode cannot be a reserved opcode')
  header = struct.pack('!B', ((fin << 7)
                              | (0 << 6)
                              | (0 << 5)
                              | (0 << 4)
                              | opcode))
  if masking_key:
    mask_bit = 1 << 7
  else:
    mask_bit = 0

  length = len(data)
  if length < 126:
    header += struct.pack('!B', (mask_bit | length))
  elif length < (1 << 16):
    header += struct.pack('!B', (mask_bit | 126)) + struct.pack('!H', length)
  elif length < (1 << 63):
    header += struct.pack('!B', (mask_bit | 127)) + struct.pack('!Q', length)
  else:
    raise

  body = data
  return bytes(header + body)


def parse_headers(data):
  headers = {}
  lines = data.splitlines()
  for l in lines:
    parts = l.split(": ", 1)
    if len(parts) == 2:
      headers[parts[0]] = parts[1]
  headers['code'] = lines[len(lines) - 1]
  return headers


def handshake(client):
  data = client.recv(1024)
  headers = parse_headers(data)
  digest = create_hash(
    headers['Sec-WebSocket-Key']
  )
  encoded_data = base64.b64encode(digest)
  shake = "HTTP/1.1 101 Web Socket Protocol Handshake\r\n"
  shake += "Upgrade: WebSocket\r\n"
  shake += "Connection: Upgrade\r\n"
  shake += "Sec-WebSocket-Location: ws://%s/stuff\r\n" % (headers['Host'])
  shake += "Sec-WebSocket-Accept: %s\r\n\r\n" % encoded_data
  return client.send(shake)


def handle(client_obj, addr):
  global clients
  client = client_obj.connection
  handshake(client)
  lock = threading.Lock()
  while 1:
    try:
      recv_data(client)

    except:

       print 'Client closed:', addr
       lock.acquire()
       clients.remove(client_obj)
       lock.release()
       client.close()
       break

def start_server():
  global clients
  s = socket.socket()
  s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  s.bind(('', 31415))
  s.listen(10)
  while 1:
    conn, addr = s.accept()
    print 'Connection from:', addr
    newClient = Client(conn)
    clients.append(newClient)
    threading.Thread(target=handle, args=(newClient, addr)).start()

def set_inputfile():
  try:
    inputfile = ""
    f = open(sys.argv[1], 'r')
    for line in f.readlines():
      inputfile += line
    return inputfile
  except:
    return ""

inputfile = set_inputfile()
p = Popen("./solver.native", stdout=PIPE, stdin=PIPE, stderr=STDOUT)
start_server()
