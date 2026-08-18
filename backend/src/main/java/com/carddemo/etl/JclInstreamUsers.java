package com.carddemo.etl;

import com.carddemo.etl.records.ReferenceRecords.SecUser;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Extracts the in-stream SYSUT1 user records from {@code DUSRSECJ.jcl}.
 * There is no {@code app/data/usrsec} file — the 10 seed users exist only as
 * 80-byte in-stream data between {@code //SYSUT1   DD *} and {@code /*},
 * laid out per CSUSR01Y (plan §2.3).
 */
public final class JclInstreamUsers {

    private JclInstreamUsers() {
    }

    public static List<SecUser> parse(Path jclFile) {
        List<String> lines;
        try {
            lines = Files.readAllLines(jclFile);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read " + jclFile, e);
        }
        List<SecUser> users = new ArrayList<>();
        boolean inStream = false;
        for (String line : lines) {
            if (!inStream) {
                if (line.startsWith("//SYSUT1") && line.contains("DD *")) {
                    inStream = true;
                }
                continue;
            }
            if (line.startsWith("/*") || line.startsWith("//")) {
                break;
            }
            users.add(SecUser.parse(line));
        }
        if (users.isEmpty()) {
            throw new IllegalStateException("no in-stream SYSUT1 user records found in " + jclFile);
        }
        return users;
    }
}
