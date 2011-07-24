
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

#ifndef CLIENT_H
#define CLIENT_H

typedef int CLIENT_Client;

/// CLIENT_Open - start a new client.
///
/// Returns -1 on error. The client should be closed when you are done using
/// it with the CLIENT_Close function.
CLIENT_Client CLIENT_Open();

/// CLIENT_Close - close a client
///
/// Close a client.
void CLIENT_Close(CLIENT_Client client);

/// CLIENT_Write - send a character to a client
///
/// Send a character to a client.
void CLIENT_Write(CLIENT_Client client, char c);

/// CLIENT_Read - read characters from a client
///
/// Read characters from a client. This returns a static null terminated
/// buffer which will be overwritten then next time this function is called.
///
/// Returns an empty string if there is no more input from the client.
char* CLIENT_Read(CLIENT_Client client);

#endif//CLIENT_H

