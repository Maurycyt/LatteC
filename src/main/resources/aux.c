#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Memory management

void registerString(void * ptr);
void registerArray(void * ptr, bool containsPointers);
void registerObject(void * ptr, int64_t classID);
void registerObjectPointerMemberOffset(int64_t classID, int64_t offset);

void increaseRefCount(void * ptr);
void decreaseRefCount(void * ptr);
void clearUnboundPointers();

// Predefined

void printInt(int64_t value) {
    printf("%ld\n", value);
}

int64_t readInt() {
    int64_t result;
    void * line = NULL;
    size_t size;

    scanf("%ld", &result);
    getline((char * *)&line, &size, stdin);
    free(line);
    return result;
}

void error() {
    printf("runtime error\n");
    exit(1);
}

void printString(void * str) {
    printf("%s\n", (char *)str);
}

void * readString() {
    void * line = NULL;
    size_t size;
    ssize_t bytesRead = getline((char * *)&line, &size, stdin);
    if (bytesRead == -1) {
        printf("Expected input, got EOF\n");
        exit(1);
    }
    line = realloc(line, bytesRead);
    ((char *)line)[bytesRead - 1] = '\0';
    registerString(line);
    return line;
}

// Auxiliary

void * concatenateStrings(void * str1, void * str2) {
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    void * result = malloc(len1 + len2 + 1);
    strcpy(result, str1);
    strcat(result, str2);
    return result;
}

bool compareStrings(void * str1, void * str2) {
    return strcmp(str1, str2) == 0;
}
